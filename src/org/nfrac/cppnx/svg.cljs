(ns org.nfrac.cppnx.svg
  (:require [org.nfrac.cppnx.core :as cppnx]
            [goog.dom :as dom]
            [reagent.core :as reagent :refer [atom]]
            [clojure.string :as str]
            [clojure.core.async :as async :refer [<! put!]]))

(defn indexed [xs] (map-indexed vector xs))

(def dragging (atom nil))

;; prevent scrolling when dragging
(defonce window-touchmove
  (aset js/window "ontouchmove"
        (fn [e]
          (when @dragging
            (.preventDefault e)))))

(defn offset-from-svg
  [e]
  (let [el (dom/getElementByClass "cppn-graph")
        r (.getBoundingClientRect el)]
    [(- (.-clientX e) (.-left r))
     (- (.-clientY e) (.-top r))]))

(defn abs [x] (if (neg? x) (- x) x))

(defn find-node-at-xy
  [by-node x y rx ry]
  (some (fn [[node info]]
          (let [nx (:x info)
                ny (:y info)]
            (when (and (< (abs (- nx x)) rx)
                       (< (abs (- ny y)) ry))
              node)))
        by-node))

(defn valid-drop?
  [cppn from to]
  (let [final? (cppnx/finals cppn)
        input? (:inputs cppn)]
    (and from to (not= from to)
         (not (and (final? from) (final? to)))
         (not (and (input? from) (input? to))))))

(defn cppn-svg
  [cppn selection event-c]
  (let [strata (cppnx/cppn-strata cppn)
        zerod? (:zerod cppn #{})
        row-px 70
        height-px (* row-px (count strata))
        width-px 400
        radius-x (/ width-px 18)
        radius-y (* row-px 0.2)
        by-node (into
                 {}
                 (for [[row nodes] (indexed strata)
                       [j node] (indexed (sort nodes))]
                   [node {:x (+ (* width-px (/ (+ j 0.5) (inc (count nodes))))
                                (* (mod row 3) (* width-px 0.02)))
                          :y (* row-px (+ row 0.5))
                          :deps (-> cppn :edges (get node))
                          :label (-> cppn :nodes (get node node) name)}]))
        drag-move (fn [e]
                    (when @dragging
                      (let [[x y] (offset-from-svg e)]
                        (js/requestAnimationFrame
                         (fn [_]
                           (let [targ (find-node-at-xy by-node x y radius-x radius-y)
                                 ok? (when targ
                                       (valid-drop? cppn (:node @dragging) targ))]
                             (swap! dragging assoc :at [x y]
                                    :target (when ok? targ))))))))
        drop (fn [e]
               (when (:target @dragging)
                 (put! event-c {:event :link
                                :from (:node @dragging)
                                :to (:target @dragging)}))
               (reset! dragging nil))
        bg-click (fn [e]
                   (.preventDefault e)
                   (put! event-c {:event :select
                                  :node nil}))]
    [:svg.cppn-graph
     {:style {:width "100%"
              :height (str height-px "px")
              :border "solid 1px black"
              :font-size "12px"}
      :onMouseMove drag-move
      :onTouchMove (fn [e]
                     (drag-move (aget (.-changedTouches e) 0)))
      :onMouseLeave (fn [e] (reset! dragging nil))
      :onMouseUp drop
      :onTouchEnd (fn [e]
                    (drop (aget (.-changedTouches e) 0)))
      :onClick bg-click}
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
           :stroke (if (:target @dragging) "#0f0" "#888")
           :stroke-width 2}]))
     ;; nodes
     (into
      [:g]
      (for [[node info] by-node
            :let [drag-start (fn [e]
                               (let [[x y] (offset-from-svg e)]
                                 (reset! dragging {:node node
                                                   :at [x y]})))
                  click (fn [e]
                          (.preventDefault e)
                          (.stopPropagation e)
                          (put! event-c {:event :select
                                         :node node}))]]
        [:g
         [:ellipse
          {:cx (:x info)
           :cy (:y info)
           :rx radius-x
           :ry radius-y
           :fill (if (= selection node) "#bdf" "#eee")
           :stroke (if (zerod? node) "#000"
                     (if (= selection node) "#888" "#ddd"))
           :stroke-width (if (zerod? node) "4" "1")
           ;; note - SVG doesn't support actual drag&drop events
           :onMouseDown drag-start
           :onTouchStart (fn [e]
                           (drag-start (aget (.-changedTouches e) 0)))
           :onClick click
           :style {:cursor "alias"
                   :user-select "none"}}]
         [:text
          {:style {:pointer-events "none"}
           :text-anchor "middle"
           :x (:x info)
           :y (+ (:y info) 4)}
          (str (:label info))]]))]))
