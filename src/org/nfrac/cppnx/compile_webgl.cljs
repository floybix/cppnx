(ns org.nfrac.cppnx.compile-webgl
  (:require [org.nfrac.cppnx.core :as cppnx]
            [gamma.api :as g]))

(defmulti node-glsl
  (fn [type sum weight]
    type))

(defmethod node-glsl :linear
  [_ sum weight]
  (g/div sum weight))

(defmethod node-glsl :sine
  [_ sum _]
  (g/sin (g/* sum (* 3.1415 2))))

(defmethod node-glsl :gaussian
  [_ sum _]
  (-> (g/* sum 2.5)
      (g/pow 2.0)
      (g/* -1.0)
      (g/exp)
      (g/* 2.0)
      (g/- 1.0)))

(defmethod node-glsl :sigmoid
  [_ sum _]
  (->
   (g/div
    1.0
    (-> (g/* sum -4.9)
        (g/exp)
        (g/+ 1.0)))
   (g/* 2.0)
   (g/- 1.0)))

(defmethod node-glsl :sawtooth
  [_ sum _]
  (let [a (g/fract sum)
        ;; could use just 'fract' but let's do a little smoothing
        peak 0.975
        truncd (g/min a peak)
        over (g/- a truncd)]
    (-> (g/- truncd (g/* over (/ peak (- 1.0 peak))))
        (g/* 2.0)
        (g/- 1.0))))

(defn build-cppn-glsl-vals
  [cppn in-exprs w-exprs]
  (let [strata (cppnx/cppn-strata cppn)
        sorted-nids (apply concat (rest strata))
        node-exprs (reduce (fn [m nid]
                             (let [node-type (get-in cppn [:nodes nid] :linear)
                                   deps (keys (get-in cppn [:edges nid]))
                                   sum (->> deps
                                            (map (fn [k]
                                                   (let [w (get w-exprs [nid k])]
                                                     (g/* w (get m k)))))
                                            (reduce g/+))
                                   sumw (->> deps
                                             (map (fn [k]
                                                    (g/abs (get w-exprs [nid k]))))
                                             (reduce g/+))
                                   expr (node-glsl node-type sum sumw)]
                                (assoc m nid expr)))
                           in-exprs
                           ;; topological sort
                           sorted-nids)]
    node-exprs))
