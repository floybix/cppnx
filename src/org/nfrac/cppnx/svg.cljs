(ns org.nfrac.cppnx.svg
  (:require [org.nfrac.cppnx.core :as cppnx]
            [reagent.core :as reagent :refer [atom]]
            [clojure.string :as str]))

(defn indexed [xs] (map-indexed vector xs))

(defn cppn-svg
  [cppn]
  (let [strata (cppnx/cppn-strata cppn)
        row-px 80
        height-px (* row-px (inc (count strata)))
        width-px 400
        radius-x (/ width-px 18)
        radius-y (* row-px 0.2)
        by-node (into
                 {}
                 (concat
                  (for [[row nodes] (indexed strata)
                        [j node] (indexed (sort nodes))]
                    [node {:x (+ (* width-px (/ (inc j) (inc (count nodes))))
                                 (* (mod row 3) (* width-px 0.02)))
                           :y (* row-px (+ row 0.5))
                           :deps (-> cppn :edges (get node))
                           :label (-> cppn :nodes (get node node) name)}])
                  (for [[j [node dep]] (indexed (:out-nodes cppn))]
                    [node {:x (* width-px (/ (inc j) (inc (count (:out-nodes cppn)))))
                           :y (* row-px (+ (count strata) 0.5))
                           :deps {dep 1.0}
                           :label (name node)}])))]
    [:svg.cppn-graph
     {:view-box (str "0 0 " width-px " " height-px)
      :style {:width "100%"
              :height (str height-px "px")
              :border "solid 1px black"
              :font-size "12px"}}
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
     ;; nodes
     (into
      [:g]
      (for [[node info] by-node]
        [:g
         [:ellipse
          {:cx (:x info)
           :cy (:y info)
           :rx radius-x
           :ry radius-y
           :fill "#eee"
           :stroke "#ddd"
           :stroke-width "1"}]
         [:text
          {:text-anchor "middle"
           :x (:x info)
           :y (+ (:y info) 4)}
          (str (:label info))]]))]))
