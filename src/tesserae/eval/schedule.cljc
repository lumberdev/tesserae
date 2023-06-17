(ns tesserae.eval.schedule
  (:require [instaparse.core :as i :include-macros true]
            [instaparse.transform :as it]
            [clojure.string :as str]
            [tick.core :as t]
            [tick.timezone]
            [tick.locale-en-us]
            [stuffs.util :as su]))

(def task-grammar
  "<expr> = perms
  <perms> = ((rord? tatin? owners?) | (tatin? ?rord owners?) | (owners? tatin? ?rord) | (owners? ?rord ?tatin))
  <rord> = repeat | day
  <tatin> = time-at | time-in
  <word> = #'\\w+'
  owner = #'@\\w+'
  owners = owner+
  repeat = (<'every'> (period | day)) | period-interval
  period = (period-number? period-unit)
  period-number = #'\\d+'
  period-unit = period-seconds | period-minutes | period-hours | period-days | period-weeks | period-months | period-years
  period-seconds = 's' | 'sec' | 'secs' | 'second' | 'seconds'
  period-minutes = 'm' | 'min' | 'mins' | 'minute' | 'minutes'
  period-hours = 'h' | 'hour' | 'hours'
  period-days = 'day' | 'days'
  period-weeks = 'week' | 'weeks'
  period-months = 'month' | 'months'
  period-years = 'year' | 'years'
  period-interval = 'hourly' | 'daily' | 'weekly' | 'monthly' | 'yearly'
  day = 'mon'|'tue'|'wed'|'weds'|'thu'|'thurs'|'fri'|'sat'|'sats'|'sun'|'monday'|'tuesday'|'wednesday'|'thursday'|'friday'|'saturday'|'sunday'
  offset = 'tom' | 'tomorrow'
  time-in = <'in'>? period
  time-at = offset? <'at'>? (day-hour (<':'> day-hour-minutes)? am-pm?)
  am-pm = 'a' | 'am' |'p'| 'pm'
  day-hour = #'[0-9]' | #'[0-1][0-9]' | #'2[0-4]'
  day-hour-minutes = #'[0-9]' | #'[0-5][0-9]'
  "
  )

(i/defparser task-parser
  task-grammar
  :auto-whitespace :standard)

(defn- tagged [tag f]
  (fn [& args]
    [tag (apply f args)]))

(defn- period [n u]
  (case u
    (:seconds :minutes :hours) (t/new-duration n u)
    (:days :weeks :months :years) (t/new-period n u)))

(defn- new-time [h m am-pm]
  (let [hour-adjusted (cond-> h
                        (= am-pm :pm) (+ 12))]
    (t/new-time hour-adjusted m)))

(defn- merge-transform-results [init results]
  (reduce
    (fn [out [k vs]]
      (update out k #(do
                       #_(println out k :vs vs :current %)
                       (cond
                         (and (vector? %) (vector? vs)) (into % vs)
                         :else vs))))
    init
    results))

(defn parse [s & {:keys [default-time-at zone]
                  :or   {default-time-at (t/new-time 12 0)
                         zone            (su/current-time-zone)}}]
  (let [s (some-> s str/trim)
        p (some-> s not-empty task-parser)]
    (try
      (if (i/failure? p)
        (assoc p :errored? true)
        (some->>
          p
          not-empty
          (it/transform
            {:title            (tagged :title #(str/join " " %&))
             :owner            #(subs % 1)
             :owners           (tagged :owners #(vec %&))
             ;:repeat           (tagged :repeat #(vec %&))
             :day              (tagged :day t/parse-day)
             :day-hour         (tagged :day-hour su/parse-int)
             :day-hour-minutes (tagged :day-hour-minutes su/parse-int)
             :period           (tagged :period
                                       (fn [& args]
                                         (let [{:keys [period-number period-unit]} (into {:period-number 1} args)]
                                           (period period-number period-unit))))
             :period-number    (tagged :period-number su/parse-int)
             :period-seconds   (constantly :seconds)
             :period-minutes   (constantly :minutes)
             :period-hours     (constantly :hours)
             :period-days      (constantly :days)
             :period-weeks     (constantly :weeks)
             :period-months    (constantly :months)
             :period-years     (constantly :years)
             :period-interval  (tagged :period
                                       (fn [interval]
                                         (case interval
                                           "hourly" (t/new-duration 1 :hours)
                                           "daily" (t/new-period 1 :days)
                                           "weekly" (t/new-period 1 :weeks)
                                           "monthly" (t/new-period 1 :months)
                                           "yearly" (t/new-period 1 :years))))
             :am-pm            (tagged :am-pm
                                       (fn [s]
                                         (case s
                                           ("a" "am") :am
                                           ("p" "pm") :pm)))
             :time-at          (tagged :time-at
                                       (fn [& args]
                                         (let [{:keys [day-hour day-hour-minutes am-pm offset]
                                                :or   {day-hour-minutes 0 am-pm :am}}
                                               (into {} args)
                                               t-now (t/new-time)
                                               t     (if-not day-hour
                                                       (if offset
                                                         ;; if tomorrow use default time, otherwise nowtime
                                                         default-time-at
                                                         (t/new-time))
                                                       (let [hour-adjusted (cond-> day-hour
                                                                             (and (< day-hour 12) (= am-pm :pm)) (+ 12))]
                                                         (t/new-time hour-adjusted day-hour-minutes)))
                                               d     (if (or (t/> t-now t) offset)
                                                       (t/tomorrow)
                                                       (t/today))]
                                           (t/in (t/at d t) zone))))})
          (merge-transform-results {:text    s
                                    :time-at (t/in (t/date-time) zone)})))
      (catch #?(:clj Throwable :cljs js/Error) e
        (assoc {:parse-tree p}
          :errored? true
          :ex-msg (ex-message e))))))

(def time-formatter
  "(t/format time-formatter (t/date-time) => \"10:36AM\""
  (t/formatter "hh:mma"))

(def day-formatter
  "(t/format
    (t/formatter \"EEE\")
    (t/tomorrow))
  => \"Thu\""
  (t/formatter "EEE"))

(def date-formatter
  "(t/format date-formatter (t/date-time (t/>> (t/now) (t/new-duration 30 :days))))
  => \"Oct 21\""
  (t/formatter "MMM d"))

(def day-date-time-formatter
  "(t/format date-formatter (t/date-time (t/>> (t/now) (t/new-duration 30 :days))))
  => \"Oct 21\""
  (t/formatter "EEE, MMM d hh:mma"))

(defn fmt-day-date-time [t]
  (t/format day-date-time-formatter t))

(defn add-next-time [{:as sched :schedule/keys [from repeat next]}]
    (let [zd (t/in (t/date-time) (t/zone from))]
      (if (or (nil? repeat) (and next (t/> next zd)))
        sched
        (let [nex-t (su/ffilter
                      #(t/> % zd)
                      (iterate #(t/>> % (second repeat)) (or next from)))]
          (assoc sched :schedule/next nex-t)))))

(defn parsed->schedule [{:keys [text time-at repeat time-in errored?]}]
  (when-not errored?
    (let [ta   (t/zoned-date-time time-at)
          base #:schedule{:text text
                          :from ta
                          :next ta}]
      (if repeat
        (add-next-time (assoc base :schedule/repeat repeat))
        base))))

(defn parse->schedule [s opts]
  (parsed->schedule (parse s opts)))

(comment
  (parse "every 5 months" {:zone "America/Los_Angeles"})
  (parse "daily at 5p")
  (parse->schedule "daily at 5p" {:FOPO 1})
  (parse "in 5 hours")
  (parse "every month at 12pm")
  (parse "every hour")
  (parse "every 5s")
  )


