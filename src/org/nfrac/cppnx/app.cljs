(ns org.nfrac.cppnx.app
  (:require [org.nfrac.cppnx.core :as cppnx]
            [org.nfrac.cppnx.helpers :refer [glcanvas]]
            [org.nfrac.cppnx.webgl-image :as gl-img]
            [org.nfrac.cppnx.webgl-lines :as gl-lines]
            [monet.canvas :as c]
            [reagent.core :as reagent :refer [atom]]
            [goog.dom :as dom]
            [goog.webgl :as ggl]))

(enable-console-print!)

(def app-state
  (atom {:cppn cppnx/example-cppn}))

(defonce ui-state
  (atom {}))

(defonce undo-buffer
  (atom ()))

(defonce redo-buffer
  (atom ()))

(defn swap-advance!
  "ref = app-state"
  [ref f & more]
  ;; record state for undo
  (swap! undo-buffer conj @ref)
  (when (seq @redo-buffer)
    (reset! redo-buffer ()))
  (apply swap! ref f more))

(defn settings-pane
  [app-state ui-state]
  (let []
    [:div
     [:div.row
      [:div.col-lg-12
       [:p
        "Settings pane. "]]]]))

(defn view-pane
  [app-state ui-state]
  (let []
    [:div
     [:div.row
      [:div.col-lg-12
       [:p
        "View panel. "]
       [glcanvas
        {:style {:border "1px black"
                 :width "400px"
                 :height "300px"}}
        400 300
        [app-state]
        (fn [gl]
          (let [pgm (gl-img/setup gl @app-state)]
            (gl-img/render gl pgm (:weights @app-state))))]]]]))

(defn navbar
  [app-state ui-state]
  (let []
    [:nav.navbar.navbar-default
     [:div.container-fluid
      [:div.navbar-header
       [:a.navbar-brand {:href "https://github.com/floybix/cppnx"}
        "cppnx."]]
      [:div
       [:ul.nav.navbar-nav
        ;; step back
        [:li
         [:button.btn.btn-default.navbar-btn
          {:type :button
           :on-click
           (fn [_]
             (let [new-state (peek @undo-buffer)]
               (swap! undo-buffer pop)
               (swap! redo-buffer conj @app-state)
               (reset! app-state new-state)))
           :title "Step backward in time"
           :disabled (when (empty? @undo-buffer) "disabled")}
          [:span.glyphicon.glyphicon-step-backward {:aria-hidden "true"}]
          " Undo"]]
        ;; step forward
        (when-not (empty? @redo-buffer)
          [:li
           [:button.btn.btn-default.navbar-btn
            {:type :button
             :on-click
             (fn [_]
               (let [new-state (peek @redo-buffer)]
                 (swap! redo-buffer pop)
                 (swap! undo-buffer conj @app-state)
                 (reset! app-state new-state)))
             :title "Step forward in time"
             :disabled (when (empty? @redo-buffer) "disabled")}
            [:span.glyphicon.glyphicon-step-forward {:aria-hidden "true"}]
            " Redo"]])
        ;; timestep
        [:li
         [:p.navbar-text
          (str "Foo")]]]]]]))

(defn app-pane
  [app-state ui-state]
  [:div
   [navbar app-state ui-state]
   [:div.container-fluid
    [:div.row
     [:div.col-lg-8.col-md-6
      [view-pane app-state ui-state]]
     [:div.col-lg-4.col-md-6
      [settings-pane app-state ui-state]]]]])

(reagent/render-component [app-pane app-state ui-state]
                          (. js/document (getElementById "app")))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
