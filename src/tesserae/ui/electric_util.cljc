(ns tesserae.ui.electric-util
  (:require [hyperfiddle.electric :as e]
            [missionary.core :as m]
            [stuffs.js-interop :as j]
            [stuffs.route])
  #?(:cljs (:require-macros tesserae.ui.electric-util))
  (:import [hyperfiddle.electric Pending]))

(defmacro state-from-atom [atm]
  `(do [~atm (e/watch ~atm)]))

(defmacro state [val]
  `(let [atm# (atom ~val)] [atm# (e/watch atm#)]))


(defmacro wrap [& body] `(e/offload #(do ~@body)))

(defn listen-and-observe
  ([dom-node event-name] (listen-and-observe dom-node event-name identity {}))
  ([dom-node event-name handler] (listen-and-observe dom-node event-name handler {}))
  ([dom-node event-name handler options]
   (m/observe
     (fn [!]
       #_(! nil)
       (let [f (fn [e] (when-some [v (handler e)]
                         (! v)))]
         (.addEventListener dom-node (name event-name) f #?(:cljs (clj->js options)))
         #(.removeEventListener dom-node (name event-name) f))))))

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

#_(e/def <copy
    (<on "copy"))



#_(e/def <document-focused?
    #?(:cljs
       (<on js/window :focus (fn [e]
                               (j/call js/document :hasFocus)
                               ))))

#_(e/def clipboard+focus
    ; https://clojurians.slack.com/archives/CL85MBPEF/p1673467726964789
    #?(:cljs
       (->>
         (m/ap
           (m/?>
             (m/amb=
               <copy
               (m/relieve {}
                          (m/observe
                            (fn [!]
                              (let [handler (fn [e]
                                              (println :running (j/call js/document :hasFocus))
                                              (when (j/call js/document :hasFocus)
                                                (! e)))]
                                (.addEventListener js/window "focus" handler)
                                #(.removeEventListener js/window "focus" handler)))))))
           (m/? (doto (m/dfv) sdom/read-clipboard)))
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
  (m/sample deref
            (m/reductions {} !x
                          (m/ap (m/?> (m/relieve {}
                                                 (m/observe
                                                   (fn [!]
                                                     (add-watch !x ! (fn [! _ _ _] (! nil)))
                                                     #(remove-watch !x !)))))
                                (m/? (m/via m/cpu !x))))))

