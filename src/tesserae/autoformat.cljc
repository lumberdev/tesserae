(ns tesserae.autoformat
  (:require [lambdaisland.regal :as regal]
            [stuffs.util :as su]))

(comment
  (require 'lambdaisland.regal.parse)
  (lambdaisland.regal.parse/parse #"(?<=@)(?!\s).*"))

(def html-regex
  (regal/regex
    [:cat :start "<" [:+ :word] ">"]))

(comment
  (lambdaisland.regal.parse/parse #"^[a-z]+")
  (lambdaisland.regal.parse/parse #"^(\d|\"|\(|\[|:\w|(?!\.))"))

(def maybe-edn-regex
  (regal/regex
    [:cat :start [:alt :digit "@" "\"" "(" "[" "{" [:cat ":" :word] [:negative-lookahead "."]]]))

(defn likely-edn? [s]
  (boolean (not-empty (re-find maybe-edn-regex s))))

(defn valid-edn? [s]
  (boolean
    (and
      ;; short circuit with this regex
      (likely-edn? s)
      (try
        (su/read-edn s)
        (catch #?(:cljs js/Error
                  :clj  Exception) _
          false)))))

(def starts-with-text-regex
  (regal/regex [:cat :start [:not ["0" "9"] "."]]))

(defn likely-text? [s]
  (boolean (not-empty (re-find starts-with-text-regex s))))

(defn edn-ize [s]
  (cond
    (likely-edn? s) s
    (likely-text? s) (pr-str s)
    :else s))

(comment
  (edn-ize "fooo")
  ;=> "\"fooo\""
  (edn-ize "\"fooo\"")
  ;=> "\"fooo\""
  (edn-ize "13")
  ;=> "13"
  (edn-ize "@c4")
  ; selection
  ; => "\"@c4\""
  (edn-ize "(ren")
  ;=> "(ren"
  ; not perfect but for syntax errors clj error messages could be more helpful
  )

