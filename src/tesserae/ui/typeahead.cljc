(ns tesserae.ui.typeahead
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [stuffs.keybind :as keybind]
            [tesserae.ui.electric-util :as eu :include-macros true]))

(e/defn Typeahead
  [{:keys [placeholder autofocus? input-value
           on-pick on-create on-blur on-esc
           template-fn suggestions-fn
           container-class container-style
           input-class input-style
           suggestions-container-style suggestions-container-class
           suggestion-class
           selected-suggestion-class]
    :or   {input-value ""}}]
  (e/client
    (let [[!show-suggestions? show-suggestions?] (eu/state autofocus?)
          [!input input] (eu/state input-value)]
      (e/server
        (let [!suggestions      (atom nil)
              suggestions       (when show-suggestions?
                                  (reset! !suggestions (suggestions-fn. input)))
              suggestions-count (count suggestions)
              handle-updates    (e/fn [{:keys [input-value blur?]}]
                                  (when blur? (e/client (.blur dom/node)))
                                  (when input-value (e/client (reset! !input input-value)))
                                  true)]
          (e/client
            (let [[!idx idx] (eu/state suggestions-count)
                  idx (mod idx (inc suggestions-count))]
              (dom/div
                (dom/props {:class container-class
                            :style container-style})
                (dom/div
                  (dom/props {:class [:relative]})
                  (when (and (not-empty input) show-suggestions? (= idx suggestions-count))
                    (dom/div
                      (dom/props {:class ["text-xs" :text-slate-400 :absolute :right-full :mr-2 :bottom-1 :whitespace-pre]})
                      (dom/span (dom/props {:class [:italic]})
                                (dom/text (str "⏎ to create")))))
                  (dom/input
                    (dom/props {:class       input-class
                                :style       input-style
                                :placeholder (or placeholder "")
                                :value       input})
                    (dom/on "input"
                            (e/fn [e]
                              (reset! !input (.. e -target -value))
                              (reset! !show-suggestions? true)))
                    (dom/on "focus"
                            (e/fn [_] (reset! !show-suggestions? true)))
                    (dom/on "blur"
                            (e/fn [e]
                              (reset! !show-suggestions? false)
                              (e/server (when on-blur
                                          (some-> (on-blur. nil)
                                                  (handle-updates.)))
                                        nil)))
                    (dom/on "keydown"
                            (e/fn [e]
                              (keybind/chord-case e
                                ("enter" "tab") (do
                                                  (.preventDefault e)
                                                  (let [input-val @!input]
                                                    (e/server
                                                      (let [picked (nth @!suggestions idx nil)]
                                                        (some->
                                                          (if picked
                                                            (on-pick. {:picked         picked
                                                                       :input-value    input-val
                                                                       :handle-updates handle-updates})
                                                            (on-create. {:input-value    input-val
                                                                         :handle-updates handle-updates}))
                                                          (handle-updates.))
                                                        nil))))
                                ("up") (swap! !idx dec)
                                ("down") (swap! !idx inc)
                                ("esc") (do
                                          (reset! !show-suggestions? false)
                                          (reset! !input "")
                                          (e/server (when on-esc (on-esc. e)))))))
                    (when autofocus? (.focus dom/node) nil)))
                (when (and show-suggestions? (pos? suggestions-count))
                  (dom/div
                    (dom/props {:style suggestions-container-style
                                :class suggestions-container-class})
                    (e/server
                      (e/for [[i sug] (map-indexed vector suggestions)]
                        (let [txt (template-fn. sug)]
                          (e/client
                            (let [selected? (= i idx)]
                              (dom/div
                                (dom/props
                                  {:class [:cursor-pointer
                                           suggestion-class
                                           (when selected? selected-suggestion-class)]})
                                (dom/on "mousedown"
                                        (e/fn [e]
                                          (let [input-val @!input]
                                            (e/server
                                              (some->
                                                (on-pick. {:picked         sug
                                                           :input-value    input-val
                                                           :handle-updates handle-updates})
                                                (handle-updates.))
                                              nil))))
                                (if (string? txt)
                                  (dom/text txt)
                                  txt)))))))))))))))))



(comment
  (e/defn TypeaheadDebug []
    (e/server
      (new Typeahead
           {:placeholder    "search or create"
            :autofocus?     true
            :template-fn    (e/fn [x] (:foo x))
            :on-create      (e/fn [_])
            :on-pick        (e/fn [_])
            :on-blur        (e/fn [_])
            :on-esc         (e/fn [_])
            :suggestions-fn (e/fn [q]
                              (get-suggestions q))}))))