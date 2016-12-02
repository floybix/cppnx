(ns org.nfrac.cppnx.helpers
  (:require [goog.dom]
            [goog.dom.classes]
            [goog.style :as style]
            [goog.events :as events]
            [reagent.core :as reagent :refer [atom]]))

;;; standard 2d canvas

(defn- canvas$call-draw-fn
  [component]
  ;; argv contains entire hiccup form, so it's shifted one to the right.
  (let [[_ _ _ _ _ draw] (reagent/argv component)]
    (draw (-> component
              reagent/dom-node
              (.getContext "2d")))))

(defn canvas [_ _ _ _ _]
  (reagent/create-class
   {:component-did-mount #(canvas$call-draw-fn %)

    :component-did-update #(canvas$call-draw-fn %)

    :display-name "canvas"
    :reagent-render (fn [props width height canaries _]
                      ;; Need to deref all atoms consumed by draw function to
                      ;; subscribe to changes.
                      (doseq [v canaries]
                        (when (satisfies? IDeref v)
                          @v))
                      [:canvas (assoc props
                                      :width width
                                      :height height)])}))

;;; webGL canvas

(defn- glcanvas$call-draw-fn
  [component gl]
  ;; argv contains entire hiccup form, so it's shifted one to the right.
  (let [[_ _ _ _ _ draw] (reagent/argv component)]
    (when gl (draw gl))))

(defn glcanvas [_ _ _ _ _]
  (let [state (clojure.core/atom {})]
    (reagent/create-class
      {:component-did-mount
       (fn [component]
         (swap! state assoc :gl (-> component
                                    reagent/dom-node
                                    (.getContext "webgl")))
         (-> component reagent/dom-node
             (.addEventListener "webglcontextlost"
                                (fn [e]
                                  (println "context lost!"))))
         (glcanvas$call-draw-fn component (:gl @state)))

       :component-did-update
       (fn [component]
         (glcanvas$call-draw-fn component (:gl @state)))

       :component-will-unmount
       (fn [component]
         (swap! state dissoc :gl))

       :display-name "glcanvas"

       :reagent-render (fn [props width height canaries draw]
                      ;; Need to deref all atoms consumed by draw function to
                      ;; subscribe to changes.
                         (doseq [v canaries]
                           (when (satisfies? IDeref v)
                             @v))
                         [:canvas (assoc props
                                         :width width
                                         :height height)])})))
