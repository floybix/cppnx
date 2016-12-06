(ns org.nfrac.cppnx.app
  (:require [org.nfrac.cppnx.core :as cppnx]
            [org.nfrac.cppnx.helpers :refer [glcanvas]]
            [org.nfrac.cppnx.webgl-image :as gl-img]
            [org.nfrac.cppnx.webgl-lines :as gl-lines]
            [org.nfrac.cppnx.webgl-trace :as gl-trace]
            [org.nfrac.cppnx.share :as share]
            [org.nfrac.cppnx.svg :as svg]
            [org.nfrac.cppnx.compile-clj :refer [build-cppn-code]]
            [fipp.edn]
            [fipp.clojure]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [reagent-modals.modals :as reagent-modals]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]
            [goog.dom.forms :as forms]
            [clojure.core.async :as async :refer [<! put!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defonce app-state
  (atom {:cppn gl-img/start-cppn
         :snapshots ()}))

(defonce mutants-state
  (atom {:mutants []}))

(def init-ui-state
  {:selection nil
   :perturbation 0.5
   :seconds-per-move 1.3
   :scrub 0
   :scrub-detail 0
   :show-mutants? true
   :n-mutants 12})

(defonce ui-state
  (atom init-ui-state))

(defonce gl-cache
  (atom {:vertex-glsl ""
         :fragment-glsl ""
         :img-data nil}))

(def gl-canvas-class "cppnx-main-canvas")
(def gl-snap-canvas-class "cppnx-snap-canvas")
(def gl-mutant-canvas-class "cppnx-mutant-canvas")

(defn gl-setup
  [gl cppn]
  (case (or (:domain cppn) :image)
    :image (gl-img/setup gl cppn)
    :lines (gl-lines/setup gl cppn)
    :trace (gl-trace/setup gl cppn)))

(defn gl-render
  [info ws]
  (case (or (:domain info) :image)
    :image (gl-img/render info ws)
    :lines (gl-lines/render info ws)
    :trace (gl-trace/render info ws)))

(defn render-to-img-data
  [cppn canvas-el-class and-hide?]
  (when-let [el (dom/getElementByClass canvas-el-class)]
    (let [_ (classes/swap el "hidden" "show")
          gl (.getContext el "webgl")
          info (gl-setup gl cppn)]
      (gl-render info (cppnx/cppn-weights cppn))
      (let [data (.toDataURL el)]
        (when and-hide?
          (classes/swap el "show" "hidden"))
        data))))

(defn generate-mutants
  [cppn ui-state]
  (let [perturbation (:perturbation @ui-state)
        n-mut (:n-mutants @ui-state)
        ms (for [i (range n-mut)]
             (let [c (if (>= i (* n-mut 0.4))
                       (cond
                         (< (rand) 0.33)
                         (cppnx/mutate-add-node cppn)
                         (< (rand) 0.5)
                         (cppnx/mutate-add-conn cppn)
                         :else
                         (cppnx/mutate-rewire-conn cppn))
                       cppn)]
               (cppnx/randomise-weights c perturbation nil)))]
    (vec ms)))

(defn render-mutants
  [cppns]
  (mapv (fn [cppn]
          {:cppn cppn
           :img-data (render-to-img-data cppn gl-mutant-canvas-class true)})
        cppns))

(defn generate-and-render-mutants!
  []
  (let [cppn (:cppn @app-state)
        muts (generate-mutants cppn ui-state)]
    (swap! mutants-state assoc :mutants (render-mutants muts))))

(defn on-new-cppn! []
  (generate-and-render-mutants!))

(defn swap-advance!
  [app-state f & more]
  (let [x (swap! app-state #(-> (apply f % more)
                                (update :cppn cppnx/trunc-precision 7)))
        uri (share/uri-with-cppn (:cppn x))]
    (.pushState js/history (hash x) "cppnx" uri)
    (on-new-cppn!)
    x))

(def all-domains [:image :lines :trace])

(defn init-cppn
  [domain]
  (case (or domain :image)
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
        (swap! app-state assoc :cppn c)
        (on-new-cppn!)))))

(defn snapshot!
  [app-state ui-state]
  (let [cppn (:cppn @app-state)]
    (when-not (contains? (set (map :cppn (:snapshots @app-state)))
                         cppn)
      (let [data (render-to-img-data cppn gl-snap-canvas-class true)]
        (swap! app-state update :snapshots conj
          {:img-data data
           :cppn cppn})))))

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
        wpf (* (min at-frac 0.999) (dec (count wp)))
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
  (when-let [gl-info-ref (:gl-info-ref @ui-state)]
    (swap! gl-info-ref assoc :stop! true)
    (when-let [weights (:weights (:tour @gl-info-ref))]
      (swap-advance! app-state update :cppn cppnx/set-cppn-weights weights)))
  (swap! ui-state dissoc :animating? :gl-info-ref))

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

(defn perturbation-slider
  [ui-state]
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
   [:label "overhaul"]])

(defn tour-init-panel
  [ui-state]
  [:div.panel.panel-default
   [:div.panel-body
    [:div
     "Go on a "
     [:button.btn.btn-success
      {:on-click (fn [e]
                   (tour-start! app-state ui-state 3))}
      "Weight-space tour"]
     [:span.small.text-muted
      " ...oh btw, you can select a node to vary only its incoming edges."]]
    [perturbation-slider ui-state]]])

(defn tour-pane
  [ui-state]
  [:div
   (if (:animating? @ui-state)
     [tour-controls app-state ui-state]
     [tour-init-panel ui-state])])

(defn topology-controls
  [app-state ui-state]
  [:div.panel.panel-default
   [:div.panel-heading
    [:b "Random changes"]
    [:span.small.text-muted
     " ...or just choose one of the mutants shown above."]]
   [:div.panel-body
    [:div.btn-group.btn-group-justified
     [:div.btn-group
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (swap-advance! app-state update :cppn
                                   cppnx/mutate-add-node))}
       "Add node"]]
     [:div.btn-group
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (swap-advance! app-state update :cppn
                                   cppnx/mutate-add-conn))}
       "Add link"]]
     [:div.btn-group
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (swap-advance! app-state update :cppn
                                   cppnx/mutate-rewire-conn))}
       "Rewire"]]
     [:div.btn-group
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (swap-advance! app-state update :cppn
                                   cppnx/randomise-weights
                                   (:perturbation @ui-state)
                                   (:selection @ui-state)))}
       "Weights"]]]]])

(defn conj-to-set-key [m k x]
  (update m k #(conj (or % #{}) x)))

(defn disj-from-set-key [m k x]
  (let [s (disj (get m k) x)]
    (if (empty? s)
      (dissoc m k)
      (assoc m k s))))

(defn node-controls
  [app-state ui-state sel fun-node?]
  (let [cppn (:cppn @app-state)
        fun-node? (contains? (:nodes cppn) sel)
        output? (contains? (:outputs cppn) sel)
        zerod? (contains? (:zerod cppn) sel)]
    [:div.panel.panel-primary
     [:div.panel-heading
      [:b "Selected node"]
      [:span.small
       " weight mutations will be limited to its (incoming) edges."]]
     [:div.panel-body
      [:div.form-inline
       (when (or fun-node? output?)
        [:div.checkbox
         {:style {:padding "5px"}
          :class (if zerod? "bg-danger" nil)}
         [:label [:input {:type :checkbox
                          :checked (when zerod? true)
                          :on-change (fn [e]
                                      (swap! app-state update-in [:cppn]
                                             (fn [cppn]
                                               (if zerod?
                                                 (disj-from-set-key cppn :zerod sel)
                                                 (conj-to-set-key cppn :zerod sel)))))}]
          " Override (0)"]])
       (when fun-node?
         [:button.btn.btn-default
          {:style {:margin "0 1ex"}
           :on-click (fn [e]
                       (swap-advance! app-state update :cppn
                                      cppnx/delete-node sel)
                       (swap! ui-state assoc :selection nil))}
          "Delete"])
       (when fun-node?
         [:div.form-group
          [:label "Function: "]
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
       (when (or fun-node? output?)
         [:span {:style {:margin-left "2em"}}
          " Incoming edge weights:"])
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
               ;; square-root transformed on both pos/neg sides
               :min -4 ;; effectively -16
               :max 4 ;; effectively 16
               :step 0.005
               :value (* (Math/sqrt (Math/abs w)) (if (neg? w) -1 1))
               :on-change (fn [e]
                            (let [z (-> e .-target forms/getValue js/parseFloat)
                                  x (* z z (if (neg? z) -1 1))]
                              (swap! app-state assoc-in [:cppn :edges sel from-node]
                                     x)))}]]))]]]))

(defn code-pane-content
  [app-state ui-state]
  [:div
   [:h4 "EDN"]
   [:pre
    (with-out-str
      (fipp.edn/pprint (:cppn @app-state)))]
   [:h4 "Clojure"]
   [:pre
    (with-out-str
      (doseq [x (build-cppn-code (:cppn @app-state))]
        (fipp.clojure/pprint x)))]
   [:h4 "GLSL"]
   [:h5 "Vertex shader"]
   [:pre
    (:vertex-glsl @gl-cache)]
   [:h5 "Fragment shader"]
   [:pre
    (:fragment-glsl @gl-cache)]])

(defn code-pane
  [app-state ui-state]
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
       [code-pane-content app-state ui-state]])]])

(defn settings-pane
  [app-state ui-state]
  (let [svg-events-c (async/chan)]
    (go-loop []
      (when-let [m (<! svg-events-c)]
        (let [from (:from m)
              to (:to m)
              f (case (:event m)
                  :select
                  (do
                    (swap! ui-state assoc :selection (:node m))
                    nil)
                  :link
                  (fn [s]
                    (cond
                      (get-in s [:cppn :edges from to]) ;; reversed
                      (update s :cppn cppnx/remove-edge to from)
                      (get-in s [:cppn :edges to from])
                      (update s :cppn cppnx/remove-edge from to)
                      :else
                      (update s :cppn cppnx/link-nodes from to))))]
           (when f
             (swap-advance! app-state f)))
        (recur)))
    (fn [_ _]
      (let [cppn (:cppn @app-state)]
        [:div
          ;; Selection controls
          (when-let [sel (:selection @ui-state)]
            [node-controls app-state ui-state sel])
          ;; SVG
          [:div.row
            [:div.col-lg-12
              [svg/cppn-svg cppn (:selection @ui-state) svg-events-c]]]
          [:div.text-muted
           "Hints."]
          [:ul.text-muted
           [:li "Drag from a source node to a target node to add a link."]
           [:li "Drag along an existing link to remove it."]
           [:li "Click a node to edit it, or its incoming links."]
           [:li "For black & white, click \"s\" (saturation) and then Override (0)."]
           [:li "To vary color directly, click \"h\" (hue) and then vary weights, etc."]]
          ;; Topology controls
          (when-not (:animating? @ui-state)
            [topology-controls app-state ui-state])]))))

(defn mutants-canvas
  []
  (reagent/create-class
   {:component-did-mount
    (fn [component]
      (generate-and-render-mutants!))
    :reagent-render
    (fn [_]
      [:canvas.hidden
       {:class gl-mutant-canvas-class
        :width 100
        :height 100
        :style {:width "100px"
                :height "100px"}}])}))

(defn mutants-pane
  [{:keys [n-mutants show-mutants? animating?]}]
  [:div
   [mutants-canvas]
   [:div
     [:div.row
      [:div.col-lg-12
       [:div
        [:label
         "Random mutations of weights & structure. "]
        (when show-mutants?
          [:span
           [:span.small.text-muted " ...pick a mutant!"]
           [:button.btn.btn-default.btn-lg
            {:style {:margin-left "2em"}
             :on-click (fn [e]
                         (generate-and-render-mutants!))}
            "Regenerate"]])
        [:button.btn.btn-default.btn-sm
         {:style {:margin-left "2em"}
          :on-click (fn [e]
                      (swap! ui-state update :show-mutants? #(not %)))}
         (if show-mutants? "hide" "show")]]]]
     (when show-mutants?
       [:div.row
        [:div.col-lg-12
         [perturbation-slider ui-state]]])
     (when show-mutants?
       [:div.row
        [:div.col-lg-12
         (doall
           (for [i (range n-mutants)]
             ^{:key (str "mutant" i)}
             [:div.pull-left
              [:img
               {:src (get-in @mutants-state [:mutants i :img-data])
                :style {:margin-left "2px"
                        :margin-bottom "2px"
                        :width "100px"
                        :height "100px"}
                :on-click (fn [e]
                            (let [cppn (get-in @mutants-state [:mutants i :cppn])]
                              (swap-advance! app-state assoc :cppn cppn)))}]]))]])]])

(def backdrop-style
  {:position         "fixed"
   :left             "0px"
   :top              "0px"
   :width            "100%"
   :height           "100%"
   :background-color "black"
   :opacity          0.8})

(defn view-pane
  [app-state animating?]
  (let []
    [:div
     [:div.backdrop
      {:style (cond-> backdrop-style
                (not animating?)
                (assoc :display "none"))
       :on-click (fn [e]
                   (when animating? (tour-stop! app-state ui-state)))}]
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
          (when-not animating?
            (let [el (dom/getElementByClass gl-canvas-class)
                  cppn (:cppn @app-state)
                  info (gl-setup gl cppn)]
              (gl-render info (cppnx/cppn-weights cppn))
              (swap! gl-cache assoc
                     :img-data (.toDataURL el)
                     :vertex-glsl (:vertex-glsl info)
                     :fragment-glsl (:fragment-glsl info)))))]]]]))

(defn snapshots-pane
  [app-state]
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
     :style {:width "100px"
             :height "100px"}}]
   (for [snap (:snapshots @app-state)]
     ^{:key (hash (:cppn snap))}
     [:div
      {:style {:display "inline-block"
               :margin-left "5px"}}
      [:a
       {:on-click
        (fn [e]
          (swap-advance! app-state assoc :cppn (:cppn snap)))}
       [:img
        {:src (:img-data snap)}]]])
   (when (seq (:snapshots @app-state))
     [:p.small.text-muted
      {:style {:display "inline-block"
               :width "52ex"
               :white-space "normal"
               :direction "ltr"}}
      "Click a snapshot to revisit it. "
      "Snapshots won't survive after leaving this page, but you can "
      "bookmark this page to keep the current CPPN (only)."])])

(def tweetbox-template
  [:div
    [:textarea.form-control {:field :textarea
                             :id :text
                             :rows 3
                             :maxLength 115}]
    [:p.text-muted.small
     "This text box is limited to 115 chars, to leave space for the link."]
    [:p
     "If you started from a published CPPN, reply to it! "
     "Paste the original tweet location here: "]
    [:div
     [:input.form-control
      {:field :text
       :id :reply-to-uri
       :placeholder "https://twitter.com/____/status/_____"}]]
    [:p
     "Note, a reply must include their @ handle in the message too; "
     "we'll stick it at the beginning unless you include it yourself."]
    [:div.small
     {:style {:text-align "right"}}
     [:div.checkbox
      [:label
       [:input
        {:field :checkbox
         :id :include-url?}]
       " Include URI (leave this on)"]]
     [:div.checkbox
      [:label
       [:input
        {:field :checkbox
         :id :include-img?}]
       " Include image (leave this on)"]]]])

(defn tweet-modal-content
  [doc]
  [:div
   {:style {:margin "1em"}}
   [:h3 "Publish on Twitter"]
   [:p.lead
    "All right, let's share this \"stepping stone\" "
    "so that others can, er, step on it."]
   [:p
    "The tweet will include the image and a link back to this page, "
    "plus any other text you add here. "
    "Better keep the #cppnx tag so this stuff is findable."]
   (when-not (or (:success? @doc) (:pending? @doc))
    [:div
     [bind-fields tweetbox-template doc]
     [:div
      [:button.btn.btn-primary.btn-lg
       {:style {:margin "5px"}
        :on-click (fn [e]
                    (swap! doc assoc :pending? true)
                    (share/tweet! (:cppn @app-state)
                                  (:img-data @gl-cache)
                                  (:text @doc)
                                  (:reply-to-uri @doc)
                                  (fn [tweet-info]
                                    (swap! app-state assoc :reply-info tweet-info)
                                    (swap! doc assoc :success? true))
                                  (:include-url? @doc)
                                  (:include-img? @doc)))
        :disabled (when (:pending? @doc) "disabled")}
       "Tweet!"]
      [:span
       " You'll be asked to sign in to a Twitter account to post it. "]
      [:span.small.text-muted
       [:a {:href "#"
            :on-click (fn [e]
                        (.preventDefault e)
                        (.clearCache js/OAuth))}
        "forget me"]]]])

   ;; pending
   (when (:pending? @doc)
     [:div
      [:p "Uploading, hopefully..."]
      [:div.progress
       [:div.progress-bar.progress-bar-striped.active
        {:aria-valuenow "100"
         :aria-valuemin "0"
         :aria-valuemax "100"
         :style {:width "100%"}}]]])
   ;; success
   (when (:success? @doc)
     [:div
      [:p.bg-success
       {:style {:padding "1ex"}}
       "Woohoo! "
       (let [info (:reply-info @app-state)]
         [:a {:href (share/tweet-uri info)}
          "Your tweet"])
       " was posted."]
      [:button.btn.btn-default.btn-lg
       {:style {:margin "5px"}
        :on-click (fn [e]
                    (reagent-modals/close-modal!))}
       "Close"]])])

(defn prepare-tweet!
  [app-state]
  (let [reply-info (:reply-info @app-state)
        ini-text (str (when reply-info
                        (str "@" (:screen-name reply-info) " "))
                      "something... #cppnx" \newline \newline "Evolve it:")
        doc (atom {:text ini-text
                   :include-url? true
                   :include-img? true
                   :reply-to-uri (if reply-info
                                   (share/tweet-uri reply-info)
                                   "")})]
   (reagent-modals/modal!
    [tweet-modal-content doc])))

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
             (snapshot! app-state ui-state))
           :title "Take snapshot"
           :disabled (when freeze? "disabled")}
          [:span.glyphicon.glyphicon-camera {:aria-hidden "true"}]
          " Snapshot"]]
        [:li
         [:button.btn.btn-primary.navbar-btn
          {:type :button
           :on-click
           (fn [_]
             (prepare-tweet! app-state))
           :title "Publish on Twitter"
           :disabled (when freeze? "disabled")}
          "Publish on Twitter"]]
        [:li
         [:p.navbar-text
          " "
          [:a
           {:href "https://twitter.com/hashtag/cppnx"}
           [:b "Gallery (on twitter)"]]]]]
       ;; domain
       [:form.navbar-form.navbar-left
         [:div.form-group
          [:label
           "Domain: "]
          [:select.form-control
           {:value (name (or (:domain (:cppn @app-state)) :image))
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

(defn warning-pane
  [app-state]
  (let [uri (share/uri-with-cppn (:cppn @app-state))
        uri-len (.-length uri)]
    (when (> uri-len 3300)
      [:div
       [:p.bg-danger
        "Warning: the CPPN is getting kind of big. Because it is encoded "
        "in the page location link, if is gets too big, twitter won't "
        "accept the link. Currently it is " uri-len
        " (twitter limit seems to be ~4000). " [:b "To reduce it: "]
        "You can delete any nodes that have no down-going links "
        "and try deleting those with only weak links. "
        "Or go back to something simpler. " [:br]
        [:i "Yes this really sucks and it's because I'm lazy and "
         "couldn't be bothered to set up a database."]]])))

(defn intro-pane
  []
  [:div
   [:p
    "Hi. This is a place to evolve images, like "
    [:a {:href "http://picbreeder.org/"} "picbreeder"] " and "
    [:a {:href "http://otoro.net/neurogram/"} "neurogram"] ". "
    "Hopefully you can see a pretty pic made by a CPPN, "
    [:i "Compositional Pattern-Producing Network. "]
    "It's an abstraction of biological development. Read the "
    [:a {:href "http://eplex.cs.ucf.edu/publications/2007/stanley-gpem07"} "CPPN paper"]
    " by "
    [:a {:href "http://www.cs.ucf.edu/~kstanley/"} "Ken Stanley"] "."]])

(defn app-pane
  [app-state ui-state]
  [:div
   [navbar app-state ui-state]
   [reagent-modals/modal-window]
   [:div.container-fluid
    [:div.row
     [:div.col-lg-12
      [intro-pane]
      [warning-pane app-state]]]
    [:div.row
     [:div.col-lg-12
      [snapshots-pane app-state]]]
    [:div.row
     [:div.col-lg-6.col-md-7
      [view-pane app-state (:animating? @ui-state)]
      [tour-pane ui-state]]
     [:div.col-lg-6.col-md-5
      [mutants-pane (select-keys @ui-state
                                 [:n-mutants :show-mutants? :animating?])]
      [settings-pane app-state ui-state]
      [code-pane app-state ui-state]]]]])

(reagent/render-component [app-pane app-state ui-state]
                          (. js/document (getElementById "app")))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
