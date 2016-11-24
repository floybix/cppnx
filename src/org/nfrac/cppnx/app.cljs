(ns org.nfrac.cppnx.app
  (:require [org.nfrac.cppnx.core :as cppnx]
            [org.nfrac.cppnx.helpers :refer [glcanvas]]
            [org.nfrac.cppnx.webgl-image :as gl-img]
            [org.nfrac.cppnx.webgl-lines :as gl-lines]
            [org.nfrac.cppnx.webgl-trace :as gl-trace]
            [org.nfrac.cppnx.share :as share]
            [org.nfrac.cppnx.svg :as svg]
            [fipp.edn]
            [reagent.core :as reagent :refer [atom]]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]
            [goog.dom.forms :as forms]
            [goog.webgl :as ggl]
            [clojure.core.async :as async :refer [<! put!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defonce app-state
  (atom {:cppn gl-img/start-cppn
         :snapshots ()}))

(defonce ui-state
  (atom {:selection nil
         :scrub 0
         :scrub-detail 0}))

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
  (let [x (apply swap! ref f more)
        uri (share/uri-with-cppn (dissoc (:cppn x) :inputs :outputs))]
    (.pushState js/history (hash x) "cppnx" uri)
    x))

(def all-domains [:image :lines :trace])

(defn init-cppn
  [domain]
  (case domain
    :image gl-img/start-cppn
    :lines gl-lines/start-cppn
    :trace gl-trace/start-cppn))

(defn get-uri-cppn-full
  "Fills in implied :inputs :outputs from given :domain"
  []
  (when-let [c (share/get-uri-cppn)]
    (merge (init-cppn (:domain c)) c)))

;;; side-effecting at js load time!
;;; not on-load after page load, because that flashes default cppn
(when-let [c (get-uri-cppn-full)]
  (swap! app-state assoc :cppn c))

(defonce
  onpopstate
  (set! (.-onpopstate js/window)
    (fn [e]
      (when-let [c (get-uri-cppn-full)]
        (swap! app-state assoc :cppn c)))))


(defn gl-setup
  [gl state]
  (case (:domain (:cppn @app-state))
    :image (gl-img/setup gl state)
    :lines (gl-lines/setup gl state)
    :trace (gl-trace/setup gl state)))

(defn gl-render
  [info ws]
  (case (:domain (:cppn @app-state))
    :image (gl-img/render info ws)
    :lines (gl-lines/render info ws)
    :trace (gl-trace/render info ws)))

(def gl-canvas-class "cppnx-main-canvas")
(def gl-snap-canvas-class "cppnx-snap-canvas")

(defn snapshot!
  [app-state ui-state]
  (let [el (dom/getElementByClass gl-snap-canvas-class)
        _ (classes/swap el "hidden" "show")
        gl (.getContext el "webgl")
        info (gl-setup gl @app-state)]
    (gl-render info (:ws info))
    (when-not (contains? (set (map :cppn (:snapshots @app-state)))
                         (:cppn @app-state))
      (swap! app-state update :snapshots conj
        {:img-data (.toDataURL el)
         :cppn (:cppn @app-state)}))
    (classes/swap el "show" "hidden")))

(defn animate [state step-fn draw-fn]
  (js/requestAnimationFrame
   (fn [time]
     (when-not (:stop! @state)
       (let [next-value (if (:paused? @ui-state)
                          @state
                          (swap! state step-fn))]
         (draw-fn next-value)
         (animate state step-fn draw-fn))))))

(defn tour-start!
  [app-state ui-state concurrency]
  (let [sel (:selection @ui-state)
        tour (cppnx/init-weights-tour (:cppn @app-state) 1 sel)
        gl-info-ref (clojure.core/atom {})
        el (dom/getElementByClass gl-canvas-class)
        gl (.getContext el "webgl")]
    (swap! ui-state assoc
           :animating? true
           :paused? false
           :scrub 0
           :scrub-detail 0
           :gl-info-ref gl-info-ref
           :anim-start (.getTime (js/Date.)))
    (reset! gl-info-ref
            (assoc (gl-setup gl @app-state)
                   :tour tour
                   :last-rendered (.getTime (js/Date.))))
    (animate
     gl-info-ref
     (fn [info]
       (let [time-now (.getTime (js/Date.))
             elapsed (- time-now (:last-rendered info))
             seconds-per-move 1.3
             dt (min 0.1 (/ elapsed 1000.0 seconds-per-move))
             tour (cppnx/step-weights-tour (:tour info) dt)]
         (-> info
             (assoc :tour tour
                    :last-rendered time-now))))
     (fn [info]
       (gl-render info (:weights (:tour info)))))))

(defn tour-pause!
  [ui-state]
  (let [gl-info-ref (:gl-info-ref @ui-state)
        tour (:tour @gl-info-ref)
        wp (:waypoints tour)]
    (swap! ui-state assoc :paused? true)
    (swap! gl-info-ref update :tour
           (fn [tour]
             (if (>= (:motion-frac tour) 0.1)
               ;; add waypoint and init new motions
               (-> (assoc tour :motion-frac 1.0)
                   (cppnx/step-weights-tour 0.01))
               tour)))))

(defn tour-continue!
  [ui-state]
  (swap! ui-state assoc :paused? false
         :scrub 0 :scrub-detail 0))

(defn tour-scrub!
  [ui-state]
  (when-not (:paused? @ui-state)
    (tour-pause! ui-state))
  (let [gl-info-ref (:gl-info-ref @ui-state)
        tour (:tour @gl-info-ref)
        wp (:waypoints tour)
        at-frac (+ (/ (- (:scrub @ui-state)) 1000)
                   (/ (- (:scrub-detail @ui-state)) 1000 (count wp) 0.5))
        wpf (* at-frac (dec (count wp)))
        wpi (Math/floor wpf)
        [to-w from-w] (take 2 (drop wpi wp))
        motions (reduce (fn [ms [i from to]]
                          (if (== from to)
                            ms
                            (conj ms (cppnx/motion i from to))))
                        []
                        (map vector (range) from-w to-w))
        t (- 1.0 (- wpf wpi))
        now-w (cppnx/apply-motions from-w motions t)]
    (swap! gl-info-ref assoc-in [:tour :weights] now-w)))

(defn tour-stop!
  [app-state ui-state]
  (let [gl-info-ref (:gl-info-ref @ui-state)
        weights (:weights (:tour @gl-info-ref))]
    (swap! gl-info-ref assoc :stop! true)
    (swap! ui-state dissoc :animating? :gl-info-ref)
    (swap-advance! app-state
                   update :cppn cppnx/set-cppn-weights weights)))

(defn tour-controls
  [app-state ui-state]
  [:div.panel.panel-primary
   [:div.panel-heading
    [:h4.panel-title "Weights tour"]]
   [:div.panel-body
    [:div.row
     [:div.col-xs-3
      [:div.form-inline
       [:button.btn.btn-danger
        {:on-click (fn [e]
                     (tour-stop! app-state ui-state))}
        "End"]
       (if (:paused? @ui-state)
         [:button.btn.btn-success
          {:on-click (fn [e]
                       (tour-continue! ui-state))}
          [:span.glyphicon.glyphicon-play {:aria-hidden "true"}]
          "Play"]
         [:button.btn.btn-primary
          {:on-click (fn [e]
                       (tour-pause! ui-state))}
          [:span.glyphicon.glyphicon-pause {:aria-hidden "true"}]
          "Pause"])]]
     [:div.col-xs-9
       [:div.form-horizontal
        [:label "full tour: "]
        [:input
         {:style {:display "inline-block"
                  :width "85%"}
          :type "range"
          :min -1000
          :max 0
          :value (:scrub @ui-state)
          :on-change (fn [e]
                       (let [x (-> e .-target forms/getValue)]
                         (swap! ui-state assoc :scrub x)
                         (tour-scrub! ui-state)))}]]
       [:div.form-horizontal
        [:label "detail: "]
        [:input
         {:style {:display "inline-block"
                  :width "85%"}
          :type "range"
          :min -1000
          :max 0
          :value (:scrub-detail @ui-state)
          :on-change (fn [e]
                       (let [x (-> e .-target forms/getValue)]
                         (swap! ui-state assoc :scrub-detail x)
                         (tour-scrub! ui-state)))}]]]]]])

(defn settings-pane
  [app-state ui-state]
  (let [svg-events-c (async/chan)]
    (go-loop []
      (when-let [m (<! svg-events-c)]
        (let [from (:from m)
              to (:to m)
              f (case (:event m)
                  :select
                  (fn [s]
                    (swap! ui-state assoc :selection (:node m))
                    s)
                  :link
                  (fn [s]
                    (cond
                      (get-in s [:cppn :edges from to])
                      (update s :cppn cppnx/remove-edge from to)
                      (get-in s [:cppn :edges to from])
                      (update s :cppn cppnx/remove-edge to from)
                      :else
                      (update s :cppn cppnx/link-nodes from to))))]
           (swap-advance! app-state f))
        (recur)))
    (fn [_ _]
      (let [cppn (:cppn @app-state)
            freeze? (:animating? @ui-state)
            disabled (when freeze? "disabled")]
        [:div
          ;; Weights
          (when-not (:animating? @ui-state)
            [:div.row
             [:div.col-xs-3
              [:button.btn.btn-default
               {:on-click (fn [e]
                            (swap-advance! app-state update :cppn
                                           cppnx/randomise-weights
                                           (:selection @ui-state)))
                :disabled disabled}
               "Random weights"]]
             [:div.col-xs-3
              [:button.btn.btn-default
               {:on-click (fn [e]
                            (tour-start! app-state ui-state 1))
                :disabled disabled}
               "Weight tour (ones)"]]
             [:div.col-xs-3
              [:button.btn.btn-primary
               {:on-click (fn [e]
                            (tour-start! app-state ui-state 3))
                :disabled disabled}
               "Weight tour (triples)"]]])
          ;; In-tour controls
          (when (:animating? @ui-state)
            [tour-controls app-state ui-state])
          ;; SVG
          [:div.row
            [:div.col-lg-12
              [svg/cppn-svg cppn (:selection @ui-state) svg-events-c]]]
          ;; Selection controls
          (when-let [sel (let [s (:selection @ui-state)]
                           (when (contains? (:nodes cppn) s) s))]
            [:div.row
             [:div.col-lg-12
              [:div.panel.panel-primary
               [:div.panel-heading
                [:h4.panel-title "Selected node"]]
               [:div.panel-body.form-inline
                [:button.btn.btn-default
                 {:on-click (fn [e]
                              (swap-advance! app-state update :cppn
                                             cppnx/delete-node sel)
                              (swap! ui-state assoc :selection nil))
                  :disabled (when freeze? "disabled")}
                 "Delete"]
                [:div.form-group
                 [:label "Function:"]
                 [:select.form-control
                  {:value (-> cppn :nodes (get sel) name)
                   :on-change (fn [e]
                                (let [s (-> e .-target forms/getValue)
                                      type (keyword s)]
                                  (swap! app-state assoc-in
                                         [:cppn :nodes sel] type)))}
                  (doall
                   (for [type cppnx/all-node-types]
                     [:option {:key (name type)
                               :value (name type)}
                      (name type)]))]]]]]])
          ;; Topology controls
          [:div.row
            [:div.col-xs-3
              [:button.btn.btn-default
               {:on-click (fn [e]
                            (swap-advance! app-state update :cppn
                                           cppnx/mutate-append-node))
                :disabled disabled}
               "Append node"]]
            [:div.col-xs-3
              [:button.btn.btn-default
               {:on-click (fn [e]
                            (swap-advance! app-state update :cppn
                                           cppnx/mutate-add-conn))
                :disabled disabled}
               "Add connection"]]
            [:div.col-xs-3
              [:button.btn.btn-default
               {:on-click (fn [e]
                            (swap-advance! app-state update :cppn
                                           cppnx/mutate-rewire-conn))
                :disabled disabled}
               "Rewire connection"]]]
          ;; Source
          [:div.row
            [:p
              "CPPN data"]
            [:pre
              (with-out-str (fipp.edn/pprint (:cppn @app-state)))]]]))))

(def backdrop-style
  {:position         "fixed"
   :left             "0px"
   :top              "0px"
   :width            "100%"
   :height           "100%"
   :background-color "black"
   :opacity          0.8})

(defn view-pane
  [app-state ui-state]
  (let []
    [:div
     [:div.backdrop
      {:style (cond-> backdrop-style
                (not (:animating? @ui-state))
                (assoc :display "none"))}]
     [:div.row
      [:div.col-lg-12
       [glcanvas
        {:class gl-canvas-class
         :style {:border "1px black"
                 :width "600px"
                 :height "600px"}}
        600 600
        [app-state]
        (fn [gl]
          (when-not (:animating? @ui-state)
            (let [info (gl-setup gl @app-state)]
              (gl-render info (:ws info)))))]]]]))

(defn snapshots-pane
  [app-state ui-state]
  [:div
   {:style {:width "100%"
            :margin-bottom "5px"
            :direction "rtl"
            :white-space "nowrap"
            :overflow-x "auto"
            :overflow-y "hidden"}}
   [:canvas.hidden
    {:class gl-snap-canvas-class
     :width 100
     :height 100
     :style {:border "1px black"
             :width "100px"
             :height "100px"}}]
   (for [snap (:snapshots @app-state)]
     ^{:key (hash (:cppn snap))}
     [:div
      {:style {:display "inline-block"
               :margin-left "5px"}}
      [:a
       {:on-click
        (fn [e]
          (swap! app-state assoc :cppn (:cppn snap)))}
       [:img
        {:src (:img-data snap)}]]])])

(defn navbar
  [app-state ui-state]
  (let [freeze? (:animating? @ui-state)]
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
          (str " ")]]
        [:li
         [:button.btn.btn-default.navbar-btn
          {:type :button
           :on-click
           (fn [_]
             (snapshot! app-state ui-state))
           :title "Take snapshot"
           :disabled (when freeze? "disabled")}
          [:span.glyphicon.glyphicon-camera {:aria-hidden "true"}]
          " Snapshot"]]]
       ;; domain
       [:form.navbar-form.navbar-left
         [:div.form-group
          [:label
           "Domain: "]
          [:select.form-control
           {:value (name (:domain (:cppn @app-state)))
            :on-change (fn [e]
                         (let [s (-> e .-target forms/getValue)
                               domain (keyword s)]
                           (swap! app-state assoc
                                  :cppn (init-cppn domain))))}
           (doall
            (for [domain all-domains]
              [:option {:key (name domain)
                        :value (name domain)}
               (name domain)]))]]]]]]))

(defn app-pane
  [app-state ui-state]
  [:div
   [navbar app-state ui-state]
   [:div.container-fluid
    [:div.row
     [:div.col-lg-12
      [snapshots-pane app-state ui-state]]]
    [:div.row
     [:div.col-lg-6.col-md-8
      [view-pane app-state ui-state]]
     [:div.col-lg-6.col-md-4
      [settings-pane app-state ui-state]]]]])

(reagent/render-component [app-pane app-state ui-state]
                          (. js/document (getElementById "app")))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
