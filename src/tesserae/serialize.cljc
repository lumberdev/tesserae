(ns tesserae.serialize
  (:require [tick.core :as t]
            [time-literals.read-write]
            [cognitect.transit :as transit]
            [hyperfiddle.electric :as e]
            #?(:cljs
               [java.time :refer [Period
                                  LocalDate
                                  LocalDateTime
                                  ZonedDateTime
                                  OffsetTime
                                  Instant
                                  OffsetDateTime
                                  ZoneId
                                  DayOfWeek
                                  LocalTime
                                  Month
                                  Duration
                                  Year
                                  YearMonth]]))
  #?(:clj (:import
            (java.time Period
                       LocalDate
                       LocalDateTime
                       ZonedDateTime
                       OffsetTime
                       Instant
                       OffsetDateTime
                       ZoneId
                       DayOfWeek
                       LocalTime
                       Month
                       Duration
                       Year
                       YearMonth))))


(def time-classes
  {'time/period          Period
   'time/date            LocalDate
   'time/date-time       LocalDateTime
   'time/zoned-date-time ZonedDateTime
   'time/instant         Instant
   ;;'offset-time OffsetTime
   ;;'offset-date-time OffsetDateTime
   'time/time            LocalTime
   'time/duration        Duration
   'time/year            Year
   'time/year-month      YearMonth
   'time/zone            ZoneId
   'time/day-of-week     DayOfWeek
   'time/month           Month})

(def write-handlers
  (into (or hyperfiddle.electric.impl.io/*write-handlers* {})
        (map (fn [[tick-class host-class]]
               [host-class (transit/write-handler
                             (constantly (str tick-class))
                             (fn [x]
                               #_(println :WRITING x
                                          :as (str x))
                               (str x)))]))
        time-classes))

(def read-handlers
  (into (or hyperfiddle.electric.impl.io/*read-handlers* {})
        (map (fn [[sym f]]
               [(str sym) (transit/read-handler (fn [x]
                                                  #_(println :GOT sym x :AS (f x))
                                                  (f x)))]))
        time-literals.read-write/tags))

#?(:clj (alter-var-root
          #'hyperfiddle.electric.impl.io/*write-handlers*
          (constantly write-handlers)))

#?(:clj (alter-var-root
          #'hyperfiddle.electric.impl.io/*read-handlers*
          (constantly read-handlers)))

#?(:cljs (set! hyperfiddle.electric.impl.io/*read-handlers* read-handlers))
#?(:cljs (set! hyperfiddle.electric.impl.io/*write-handlers* write-handlers))
