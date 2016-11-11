(ns org.nfrac.cppnx.animate
  (:require [org.nfrac.cppnx.core :as cppnx :refer [remap]]
            [gamma.api :as g]
            [gamma.program :as p]
            [goog.dom :as gdom]
            [goog.webgl :as ggl]))

(defn animate [app-state step-fn draw-fn]
  (js/requestAnimationFrame
   (fn [time]
     (when (:animating? @app-state)
       (let [next-value (swap! app-state step-fn)]
         (draw-fn next-value)
         (animate app-state step-fn draw-fn))))))

#_
(defn tick
  "Takes the old world value and produces a new world value, suitable
  for rendering"
  [state]
  ;; We get the elapsed time since the last render to compensate for
  ;; lag, etc.
  (let [time-now   (.getTime (js/Date.))
        elapsed    (- time-now (:last-rendered state))]))
