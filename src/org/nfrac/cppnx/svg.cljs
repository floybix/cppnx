(ns org.nfrac.cppnx.svg
  (:require [org.nfrac.cppnx.core :as cppnx]
            [goog.dom :as dom]
            [reagent.core :as reagent :refer [atom]]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [<! put!]]))

(defn indexed [xs] (map-indexed vector xs))

(def dragging (atom nil))

(defn offset-from-svg
  [e]
  (let [el (dom/getElementByClass "cppn-graph")
        r (.getBoundingClientRect el)]
    [(- (.-clientX e) (.-left r))
     (- (.-clientY e) (.-top r))]))

(defn cppn-svg
  [cppn event-c]
  (let [strata (cppnx/cppn-strata cppn)
        row-px 80
        height-px (* row-px (count strata))
        width-px 400
        radius-x (/ width-px 18)
        radius-y (* row-px 0.2)
        by-node (into
                 {}
                 (for [[row nodes] (indexed strata)
                       [j node] (indexed (sort nodes))]
                   [node {:x (+ (* width-px (/ (inc j) (inc (count nodes))))
                                (* (mod row 3) (* width-px 0.02)))
                          :y (* row-px (+ row 0.5))
                          :deps (-> cppn :edges (get node))
                          :label (-> cppn :nodes (get node node) name)}]))
        drag-move (fn [e]
                    (when @dragging
                      (let [[x y] (offset-from-svg e)]
                        (swap! dragging assoc :at [x y]))))]
    [:svg.cppn-graph
     {:style {:width "100%"
              :height (str height-px "px")
              :border "solid 1px black"
              :font-size "12px"}
      :onMouseMove drag-move
      :onTouchMove drag-move
      :onMouseLeave (fn [e] (reset! dragging nil))
      :onMouseUp (fn [e] (reset! dragging nil))}
     ;; edges
     (into
      [:g]
      (for [[node info] by-node
            [from w] (:deps info)
            :let [from-info (by-node from)]]
        [:polyline
         {:points (str/join " " [(:x info) (:y info)
                                 (:x from-info) (:y from-info)])
          :stroke (if (pos? w) "#000" "#f00")
          :stroke-width (Math/sqrt (Math/abs w))}]))
     ;; drag-in-progress arrow
     (when @dragging
       (let [info (by-node (:node @dragging))
             [x y] (:at @dragging)]
         [:polyline
          {:points (str/join " " [(:x info) (:y info)
                                  x y])
           :stroke (if (:on-target? @dragging) "#0f0" "#888")
           :stroke-width 2}]))
     ;; nodes
     (into
      [:g]
      (for [[node info] by-node
            :let [drag-start (fn [e]
                               (let [[x y] (offset-from-svg e)]
                                 (reset! dragging {:node node
                                                   :at [x y]})))
                  drag-enter (fn [e]
                               (when @dragging
                                 (.preventDefault e)
                                 (swap! dragging assoc :on-target? true)))
                  drag-touch (fn [e]
                               (if @dragging (drag-enter e) (drag-start e)))
                  drag-leave (fn [e]
                               (when @dragging
                                 (.preventDefault e)
                                 (swap! dragging assoc :on-target? false)))
                  drop (fn [e]
                         (.preventDefault e)
                         (let [from (:node @dragging)]
                           (when (not= from node)
                             (put! event-c {:event :link
                                            :from from
                                            :to node}))))
                  click (fn [e]
                          (.preventDefault e)
                          (put! event-c {:event :select
                                         :node node}))]]
        [:g
         [:ellipse
          {:cx (:x info)
           :cy (:y info)
           :rx radius-x
           :ry radius-y
           :fill "#eee"
           :stroke "#ddd"
           :stroke-width "1"
           ;; note - SVG doesn't support actual drag&drop events
           :onMouseDown drag-start
           :onMouseEnter drag-enter
           :onMouseLeave drag-leave
           :onMouseUp drop
           :onTouchStart drag-touch
           :onTouchEnd drop ;; could be leave or drop
           :onClick click
           :style {:cursor "alias"}}]
         [:text
          {:style {:pointer-events "none"
                   :user-select "none"}
           :text-anchor "middle"
           :x (:x info)
           :y (+ (:y info) 4)}
          (str (:label info))]]))]))
