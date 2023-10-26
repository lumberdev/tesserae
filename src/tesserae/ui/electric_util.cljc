(ns tesserae.ui.electric-util
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [stuffs.js-interop :as j]
            [stuffs.route]
            [stuffs.dom :as sdom]
            [contrib.missionary-contrib :as mx]
            [clojure.core :as cc])
  #?(:cljs (:require-macros tesserae.ui.electric-util))
  (:import [hyperfiddle.electric Pending]
           [missionary Cancelled]))

(defmacro state-from-atom [atm]
  `(do [~atm (e/watch ~atm)]))

(defmacro state [val]
  `(let [atm# (atom ~val)] [atm# (e/watch atm#)]))

(defmacro wrap [& body] `(e/offload #(do ~@body)))

(defn listen-and-observe
  ([dom-node event-name] (listen-and-observe dom-node event-name identity {}))
  ([dom-node event-name handler] (listen-and-observe dom-node event-name handler {}))
  ([dom-node event-name handler options]
   (m/relieve
     {}
     (m/observe
       (fn [!]
         #_(! nil)
         (let [f (fn [e] (when-some [v (handler e)]
                           (! v)))]
           (.addEventListener dom-node (name event-name) f #?(:cljs (clj->js options)))
           #(.removeEventListener dom-node (name event-name) f)))))))

(defmacro discrete-flow->electric [expr]
  `(->> ~expr
        (m/relieve {})
        (m/reductions {} nil)
        new))


(defmacro <on
  "Call the `callback` clojure function on event.
   (on! \"click\" (fn [event] ...)) "
  ([event-name] `(discrete-flow->electric (listen-and-observe js/document ~event-name identity)))
  ([event-name handler] `(discrete-flow->electric (listen-and-observe js/document ~event-name ~handler)))
  ([dom-node event-name handler] `(discrete-flow->electric (listen-and-observe ~dom-node ~event-name ~handler)))
  ([dom-node event-name handler options] `(discrete-flow->electric (listen-and-observe ~dom-node ~event-name ~handler ~options))))

(e/def <copy
  (listen-and-observe js/document "copy"))

(e/def <window-focus
  (listen-and-observe
    js/window
    "focus"
    (fn [e]
      (when (j/call js/document :hasFocus)
        e))))

(e/def window-focus
  (e/client
    (->> <window-focus
         (m/reductions {} nil)
         new)))

(e/def <document-focused?
  (mx/mix
    (listen-and-observe js/window "focus" #(j/call js/document :hasFocus))
    (listen-and-observe js/window "blur" #(j/call js/document :hasFocus))))

(e/def document-focused?
  (e/client
    (->> <document-focused?
         (m/reductions {} (j/call js/document :hasFocus))
         (m/relieve {})
         new)))

(e/def clipboard-on-window-focus
  ; https://clojurians.slack.com/archives/CL85MBPEF/p1673467726964789
  (e/client
    (->>
      (m/ap
        (m/?> <window-focus)
        ;; when clipboard is empty or not of string value
        ;; emit m/none
        (or (m/? (doto (m/dfv) sdom/read-clipboard))
            (m/amb)))
      (m/reductions {} "")
      new)))

(e/def clipboard-on-copy-and-window-focus
  ; https://clojurians.slack.com/archives/CL85MBPEF/p1673467726964789
  (e/client
    (->>
      (m/ap
        (m/?>
          (m/amb=
            <copy
            <window-focus))
        (or (m/? (doto (m/dfv) sdom/read-clipboard))
            (m/amb)))
      (m/reductions {} "")
      new)))

(defmacro set-pending [atm & body]
  `(try (do
          (e/on-unmount #(reset! ~atm false))
          ~@body)
        (catch Pending e#
          (reset! ~atm true)
          (throw e#))))

(defn async-watch [!x]
  (m/sample
    deref
    (m/reductions
      {}
      !x
      (m/ap
        (m/?> (m/relieve {}
                         (m/observe
                           (fn [!]
                             (add-watch !x ! (fn [! _ _ _] (! nil)))
                             #(remove-watch !x !)))))
        (m/? (m/via m/cpu !x))))))

(defn await-promise "Returns a task completing with the result of given promise"
  [p]
  (let [v (m/dfv)]                                          ; dataflow "atom"
    (.then p
           #(v (fn [] %))                                   ; wrap result in closure and put closure in atom
           #(v (fn [] (throw %))))                          ; delayed throw
    (m/absolve v)))

(defmacro <await-promise [p]
  `(new (e/task->cp (await-promise ~p))))
