(ns tesserae.eval.schedule
  (:require [instaparse.core :as i :include-macros true]
            [instaparse.transform :as it]
            [clojure.string :as str]
            [tick.core :as t]
            [tick.timezone]
            [tick.locale-en-us]
            [stuffs.util :as su])
  (:refer-clojure :exclude [future?]))

(def task-grammar
  "<expr> = perms
  <perms> = ((rord? tatin?) | (tatin? ?rord))
  <rord> = repeat month-day? | day
  <tatin> = time-at | time-in
  <word> = #'\\w+'
  repeat = (<'every'> (period | day | day-range)) | period-interval
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
  day-range = 'weekday' | 'weekdays'
  offset = 'tom' | 'tomorrow'
  time-in = <'in'>? period
  time-at = offset? <'at'>? (day-hour (<':'> day-hour-minutes)? am-pm?)
  am-pm = 'a' | 'am' |'p'| 'pm'
  day-hour = #'[0-9]' | #'[0-1][0-9]' | #'2[0-4]'
  day-hour-minutes = #'[0-9]' | #'[0-5][0-9]'
  month-day = <'on'? 'the'?>? (#'[0-9]' | #'[1-2][0-9]' | '30' | '31') <'st' | 'nd' | 'th'>?
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

(def weekdays
  #{t/MONDAY
    t/TUESDAY
    t/WEDNESDAY
    t/THURSDAY
    t/FRIDAY})

(declare parsed-with-month-day)

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
             ;:owner            #(subs % 1)
             ;:owners           (tagged :owners #(vec %&))
             ;:repeat           (tagged :repeat #(vec %&))
             :day              (tagged :day t/parse-day)
             :day-hour         (tagged :day-hour su/parse-int)
             :day-hour-minutes (tagged :day-hour-minutes su/parse-int)
             :day-range        (tagged :days
                                       (fn [day-range]
                                         (case day-range
                                           ("weekday" "weekdays") weekdays)))
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

             :month-day        (tagged :month-day su/parse-int)
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
                                               t-now (t/in (t/now) zone)
                                               t     (if-not day-hour
                                                       (if offset ; day offset
                                                         ;; if tomorrow use default time, otherwise nowtime
                                                         default-time-at
                                                         t-now)
                                                       (let [hour-adjusted (cond-> day-hour
                                                                             (and (< day-hour 12) (= am-pm :pm)) (+ 12))]
                                                         (-> (t/today)
                                                             (t/at (t/new-time hour-adjusted day-hour-minutes))
                                                             (t/in zone))))
                                               d     (if (or (t/> t-now t) offset)
                                                       (t/tomorrow)
                                                       (t/today))]
                                           (t/in (t/at d t) zone))))})
          (merge-transform-results {:text    s
                                    :time-at (t/in (t/date-time) zone)})
          parsed-with-month-day))
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

(defn next-times [start-t interval]
  (drop 1 (iterate #(t/>> % interval) start-t)))

(defn next-time [prev-t now-t interval]
  (su/ffilter
    #(t/> % now-t)
    (next-times prev-t interval)))

(defn days-seq
  ([] (days-seq (t/today)))
  ([start]
   (iterate #(t/>> % (t/new-period 1 :days)) start)))

(defn future? [d]
  (t/> d (t/now)))

(defn future-times [start-t interval]
  (filter future? (iterate #(t/>> % interval) start-t)))

(defn next-matching-days [start days]
  (let [day-set (su/ensure-set days)]
    (filter #(and (contains? day-set (t/day-of-week %))
                  (future? %))
            (days-seq start))))

(defn next-matching-day [day-set]
  (su/ffilter #(contains? day-set (t/day-of-week %)) (days-seq)))

(defn next-month-day [start month-day]
  {:pre [(>= 31 month-day 1)]}
  (su/ffilter #(= month-day (t/day-of-month %)) (days-seq start)))

(defn next-weekday []
  (next-matching-day weekdays))

(defn next-time-of-days [start days]
  (first (next-matching-days start days)))

(defn add-next-time [{:as sched :schedule/keys [from repeat next]}]
  (let [zd (t/in (t/now) (t/zone from))]
    (cond
      (and next (t/> next zd)) sched
      (nil? repeat) (dissoc sched :schedule/next)
      repeat (let [[tag v] repeat]
               (assoc sched
                 :schedule/next (case tag
                                  :period (next-time (or next from) zd v)
                                  (:day :days) (next-time-of-days (or next from) v)))))))

(defn parsed-with-month-day
  [{:as m :keys [text time-at repeat time-in errored? day month-day]}]
  (cond-> m
    (and month-day
         (when-let [[tag period] repeat]
           (and (= :period tag)
                (= 1 (t/months period))))) (assoc :time-at (next-month-day time-at month-day))))

(defn parsed->schedule [{:as m :keys [text time-at repeat time-in errored? day month-day]}]
  (when (and m (not errored?))
    (let [base #:schedule{:text text
                          :from time-at}]
      (cond
        repeat (add-next-time (assoc base :schedule/repeat repeat))
        time-in (-> base
                    (assoc :schedule/repeat time-in)
                    add-next-time
                    (dissoc :schedule/repeat))
        day (-> base
                (assoc :schedule/repeat [:day day])
                add-next-time
                (dissoc :schedule/repeat))
        :else (assoc base
                :schedule/next time-at)))))

(defn parse->schedule [s opts]
  (parsed->schedule (parse s opts)))

(comment
  (parse "every 5 months" {:zone "America/Los_Angeles"})
  (parse "daily at 5p")
  (parse "daily at 5p")
  (parse "daily at 6p" {:zone "America/New_York"})
  (parse "in 5 hours")
  (parse "every month at 12pm")
  (parse "every 2 days at 12pm")
  (parse "weds at 12pm")
  (parse->schedule "weds at 12pm" {})
  (parse "every hour")
  (parse->schedule "every hour" {})
  (parse "every 5s")
  (parse "in 5m")
  (parse "in 2 min" {:zone "America/New_York"})
  (parse->schedule "daily at 6p" {:zone "America/New_York"})
  (parse->schedule "at 3:25p" {})
  (parse->schedule "every hour" {})
  (parse->schedule "at 4:10p" {:zone "America/New_York"})
  (parse->schedule "in 1 hour" {:zone "America/New_York"})
  (parse->schedule "every 2s" {:zone "America/New_York"})
  (parse "every weekday" {:zone "America/New_York"})
  (parse->schedule "every weekday at 6pm" {:zone "America/New_York"})
  (parse->schedule "every month on the 13th at 6pm" {:zone "America/New_York"})
  (task-parser "every fri at 6pm")
  (parse "every fri at 6pm" {:zone "America/New_York"})
  (parse->schedule "every fri at 6pm" {:zone "America/New_York"})
  (parse->schedule "every 5 months" {:zone "America/Los_Angeles"})
  (parse->schedule "every fri on the 12th at 6pm" {:zone "America/New_York"})
  (parse "monthly on the 1st" {})
  (parse->schedule "monthly" {})
  (parse->schedule "monthly on the 1st" {})
  (parse->schedule "monthly on the 2nd" {})
  (parse->schedule "monthly on the 15th" {})
  )
