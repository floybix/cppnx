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
         :mutants []
         :snapshots ()}))

(def init-ui-state
  {:selection nil
   :perturbation 0.5
   :seconds-per-move 1.3
   :scrub 0
   :scrub-detail 0
   :n-mutants 6})

(defonce ui-state
  (atom init-ui-state))

(defn generate-mutants
  [cppn ui-state]
  (let [perturbation (:perturbation @ui-state)
        ms (for [i (range (:n-mutants @ui-state))]
             (-> (cond
                   (< (rand) 0.33)
                   (cppnx/mutate-append-node cppn)
                   (< (rand) 0.5)
                   (cppnx/mutate-add-conn cppn)
                   :else
                   (cppnx/mutate-rewire-conn cppn))
                 (cppnx/randomise-weights perturbation nil)))]
    (vec ms)))

(defn on-new-cppn!
  []
  (let [cppn (:cppn @app-state)]
    (swap! app-state assoc :mutants (generate-mutants cppn ui-state))))

(defn swap-advance!
  "ref = app-state"
  [ref f & more]
  (let [x (apply swap! ref f more)
        uri (share/uri-with-cppn (dissoc (:cppn x) :inputs :outputs))]
    (.pushState js/history (hash x) "cppnx" uri)
    (on-new-cppn!)
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
(on-new-cppn!)

(defonce
  onpopstate
  (set! (.-onpopstate js/window)
    (fn [e]
      (when-let [c (get-uri-cppn-full)]
        (swap! app-state assoc :cppn c)
        (on-new-cppn!)))))


(defn gl-setup
  [gl cppn]
  (case (:domain cppn)
    :image (gl-img/setup gl cppn)
    :lines (gl-lines/setup gl cppn)
    :trace (gl-trace/setup gl cppn)))

(defn gl-render
  [info ws]
  (case (:domain info)
    :image (gl-img/render info ws)
    :lines (gl-lines/render info ws)
    :trace (gl-trace/render info ws)))

(def gl-canvas-class "cppnx-main-canvas")
(def gl-snap-canvas-class "cppnx-snap-canvas")
(def gl-mutant-canvas-class "cppnx-mutant-canvas")

(defn snapshot!
  [app-state ui-state]
  (let [el (dom/getElementByClass gl-snap-canvas-class)
        _ (classes/swap el "hidden" "show")
        gl (.getContext el "webgl")
        info (gl-setup gl (:cppn @app-state))]
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
  (let [tour (cppnx/init-weights-tour (:cppn @app-state) concurrency
                                      (:perturbation @ui-state)
                                      (:selection @ui-state))
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
            (assoc (gl-setup gl (:cppn @app-state))
                   :tour tour
                   :last-rendered (.getTime (js/Date.))))
    (animate
     gl-info-ref
     (fn [info]
       (let [time-now (.getTime (js/Date.))
             elapsed (- time-now (:last-rendered info))
             dur-secs (:seconds-per-move @ui-state)
             dt (min 0.1 (/ elapsed 1000.0 dur-secs))
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
    [:b "Weights tour"]
    [:span.small
     " ...if you miss something, scrub back in time with the sliders."]]
   [:div.panel-body
    [:div.row
     [:div.col-xs-2
      [:div.btn-group-vertical
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
          "Pause"])
       [:button.btn.btn-danger
        {:on-click (fn [e]
                     (tour-stop! app-state ui-state))}
        "End"]]]
     [:div.col-xs-10
       [:div.form-horizontal
        [:label
         {:style {:display "inline-block"
                  :width "20%"}}
         "full tour: "]
        [:input
         {:style {:display "inline-block"
                  :width "75%"}
          :type "range"
          :min -1000
          :max 0
          :value (:scrub @ui-state)
          :on-change (fn [e]
                       (let [x (-> e .-target forms/getValue js/parseFloat)]
                         (swap! ui-state assoc :scrub x)
                         (tour-scrub! ui-state)))}]]
       [:div.form-horizontal
        [:label
         {:style {:display "inline-block"
                  :width "20%"}}
         "detail: "]
        [:input
         {:style {:display "inline-block"
                  :width "75%"}
          :type "range"
          :min -1000
          :max 0
          :value (:scrub-detail @ui-state)
          :on-change (fn [e]
                       (let [x (-> e .-target forms/getValue js/parseFloat)]
                         (swap! ui-state assoc :scrub-detail x)
                         (tour-scrub! ui-state)))}]]
       [:div.form-horizontal
         [:label
          (str (-> (:seconds-per-move @ui-state)
                   (* 100) int (/ 100))
               " sec/move ")]
         [:button.btn.btn-default.btn-sm
          {:on-click
           (fn [e]
             (swap! ui-state update :seconds-per-move #(* % 0.6667)))}
          "Faster!"]
         [:button.btn.btn-default.btn-sm
          {:on-click
           (fn [e]
             (swap! ui-state update :seconds-per-move #(* % 1.5)))}
          "Slower."]]]]]])

(defn weights-controls
  [app-state ui-state]
  [:div.panel.panel-default
   [:div.panel-heading
    [:b "Parameter changes"]
    [:span.small.text-muted
     " ...oh btw, you can select a node to vary only its incoming edges."]]
   [:div.panel-body
    [:div.row
     [:div.col-lg-12
      [:div
        {:style {:padding-left "1em"}}
        [:label "tweak"]
        [:input
         {:style {:display "inline-block"
                  :margin "1ex"
                  :width "70%"}
          :type "range"
          :min 1
          :max 100
          :value (int (* 100 (:perturbation @ui-state)))
          :on-change (fn [e]
                       (let [x (-> e .-target forms/getValue)]
                         (swap! ui-state assoc :perturbation (/ x 100))))}]
        [:label "overhaul"]]]]
    [:div.btn-group.btn-group-justified
     [:div.btn-group
      [:button.btn.btn-primary
       {:on-click (fn [e]
                    (swap-advance! app-state update :cppn
                                   cppnx/randomise-weights
                                   (:perturbation @ui-state)
                                   (:selection @ui-state)))}
       "Vary weights"]]
     [:div.btn-group
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (tour-start! app-state ui-state 1))}
       "Weight tour (x1)"]]
     [:div.btn-group
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (tour-start! app-state ui-state 3))}
       "Weight tour (x3)"]]]]])

(defn topology-controls
  [app-state ui-state]
  [:div.panel.panel-default
   [:div.panel-heading
    [:b "Structure changes"]
    [:span.small.text-muted
     " ...for an easier time, choose one of the mutants shown below the pic."]]
   [:div.panel-body
    [:p.text-muted
     (str "Just drag between nodes to add or remove links in the graph above."
          " Click a node to edit it. Or make a random mutation:")]
    [:div.btn-group.btn-group-justified
     [:div.btn-group
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (swap-advance! app-state update :cppn
                                   cppnx/mutate-append-node))}
       "Append node"]]
     [:div.btn-group
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (swap-advance! app-state update :cppn
                                   cppnx/mutate-add-conn))}
       "Random link"]]
     [:div.btn-group
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (swap-advance! app-state update :cppn
                                   cppnx/mutate-rewire-conn))}
       "Random rewire"]]]]])

(defn node-controls
  [app-state ui-state sel fun-node?]
  [:div.panel.panel-primary
   [:div.panel-heading
    [:b "Selected node"]]
   [:div.panel-body
    [:div.form-inline
     (when fun-node?
       [:button.btn.btn-default
        {:style {:margin-right "1ex"}
         :on-click (fn [e]
                     (swap-advance! app-state update :cppn
                                    cppnx/delete-node sel)
                     (swap! ui-state assoc :selection nil))}
        "Delete"])
     (when fun-node?
       [:div.form-group
        [:label "Function:"]
        [:select.form-control
         {:value (-> @app-state :cppn :nodes (get sel) name)
          :on-change (fn [e]
                       (let [s (-> e .-target forms/getValue)
                             type (keyword s)]
                         (swap-advance! app-state assoc-in
                                        [:cppn :nodes sel] type)))}
         (doall
          (for [type cppnx/all-node-types]
            [:option {:key (name type)
                      :value (name type)}
             (name type)]))]])
     [:span
      " Incoming edge weights:"]]
    (let [cppn (:cppn @app-state)]
      (doall
        (for [[from-node w] (-> cppn :edges sel sort)
              :let [from-label (name (or (get (:nodes cppn) from-node)
                                         from-node))]]
          ^{:key from-node}
          [:div
           [:label
            {:style {:display "inline-block"
                     :width "20%"}}
            (str from-label ": "
                 (-> w (* 100) int (/ 100)))]
           [:input
            {:style {:display "inline-block"
                     :width "75%"}
             :type "range"
             :min -16
             :max 16
             :step 0.01
             :value w
             :on-change (fn [e]
                          (let [x (-> e .-target forms/getValue js/parseFloat)]
                            (swap! app-state assoc-in [:cppn :edges sel from-node]
                                   x)))}]])))]])

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
                      (get-in s [:cppn :edges from to]) ;; reversed
                      (update s :cppn cppnx/remove-edge to from)
                      (get-in s [:cppn :edges to from])
                      (update s :cppn cppnx/remove-edge from to)
                      :else
                      (update s :cppn cppnx/link-nodes from to))))]
           (swap-advance! app-state f))
        (recur)))
    (fn [_ _]
      (let [cppn (:cppn @app-state)]
        [:div
          ;; Weight controls
          (when-not (:animating? @ui-state)
            [weights-controls app-state ui-state])
          ;; In-tour controls
          (when (:animating? @ui-state)
            [tour-controls app-state ui-state])
          ;; SVG
          [:div.row
            [:div.col-lg-12
              [svg/cppn-svg cppn (:selection @ui-state) svg-events-c]]]
          ;; Selection controls
          (when-let [sel (:selection @ui-state)]
            [node-controls app-state ui-state sel (contains? (:nodes cppn) sel)])
          ;; Topology controls
          (when-not (:animating? @ui-state)
            [topology-controls app-state ui-state])
          ;; Source
          [:div
           [:div.panel.panel-default
            [:div.panel-heading
              [:b
               "Data / code "]
             [:button.btn.btn-default.btn-sm
              {:on-click
               (fn [e]
                 (swap! ui-state update :show-code? #(not %)))}
              (if (:show-code? @ui-state) "hide" "show")]]
            (when (:show-code? @ui-state)
              [:div.panel-body
               [:pre
                (with-out-str
                 (fipp.edn/pprint (:cppn @app-state)))]])]]]))))

(defn mutants-pane
  [app-state n-mutants show-mutants? animating?]
  (let [mutants (:mutants @app-state)]
    [:div
     [:div.row
      [:div.col-lg-12
       [:div.checkbox
        [:label
         [:input
          {:type :checkbox
           :checked (when show-mutants? true)
           :on-change
           (fn [e]
             (let [x (-> e .-target forms/getValue)]
               (swap! ui-state update :show-mutants? #(not %))))}]
         " Show some mutants (random structure & weight changes) "]
        (when show-mutants?
          [:span
           [:span.small.text-muted " ...pick one!"]
           [:button.btn.btn-default.btn-xs
            {:style {:margin-left "2em"}
             :on-click (fn [e]
                         (let [cppn (:cppn @app-state)]
                           (swap! app-state assoc :mutants
                                  (generate-mutants cppn ui-state))))}
            "Meh..."]])]]]
     (when (and show-mutants? (not animating?))
       [:div.row
        [:div.col-lg-12
         (for [[i cppn] (map-indexed vector mutants)]
           ^{:key (str "mutant" i)}
           [:div.pull-left
            [glcanvas
             {:class gl-mutant-canvas-class
              :style {:margin-left "2px"
                      :width "100px"
                      :height "100px"}
              :on-click (fn [e]
                          (swap-advance! app-state assoc :cppn cppn))}
             100 100
             [app-state]
             (fn [gl]
                 (let [info (gl-setup gl cppn)]
                   (gl-render info (:ws info))))]])]])]))

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
            (let [info (gl-setup gl (:cppn @app-state))]
              (gl-render info (:ws info)))))]]]
     ;; pass derefd to avoid needless deref triggers
     (let [{:keys [n-mutants show-mutants? animating?]} @ui-state]
       [mutants-pane app-state n-mutants show-mutants? animating?])]))

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
        [:li
         [:button.btn.btn-default.navbar-btn
          {:type :button
           :on-click
           (fn [_]
             (snapshot! app-state ui-state)
             (swap! ui-state assoc :did-snapshot? true))
           :title "Take snapshot"
           :disabled (when freeze? "disabled")}
          [:span.glyphicon.glyphicon-camera {:aria-hidden "true"}]
          " Snapshot"]]
        (when (:did-snapshot? @ui-state) ;; draw attention on first click
          [:li
           [:p.navbar-text
            " (click one to load it)"]])]
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
                           (swap-advance! app-state assoc
                                          :cppn (init-cppn domain))))}
           (doall
            (for [domain all-domains]
              [:option {:key (name domain)
                        :value (name domain)}
               (name domain)]))]]]
       ;; right-aligned items
       [:ul.nav.navbar-nav.navbar-right
        [:li
         [:p.navbar-text
          " use browser back/forward as undo/redo!"]]]]]]))

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
