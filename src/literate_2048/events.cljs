(ns literate-2048.events
  (:require [goog.events :as events]
            [cljs.core.async :refer (chan dropping-buffer put!)]))

(defn- events->chan
  "Returns a channel c where the events of event-type that happens on the DOM
   element el will be put. If c is not supplied (chan) is used."
  ([el event-type] (events->chan el event-type (chan)))
  ([el event-type c]
     (events/listen el event-type
       (fn [e] (put! c e)))
     c))

(defn keys-chan
  "Returns a channel of :left, :up, :right and :down events sourced from arrow
   key presses."
  []
  (let [key-map {37 :left 38 :up 39 :right 40 :down}]
    (events->chan js/document goog.events.EventType.KEYUP
      (chan (dropping-buffer 1) (comp (map #(.-keyCode %))
                                      (filter (set (keys key-map)))
                                      (map key-map))))))
