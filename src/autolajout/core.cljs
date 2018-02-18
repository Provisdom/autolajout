(ns autolajout.core
  (:require cljsjs.autolayout
            [clojure.walk :as walk]
            [rum.core :as rum]
            [cljs.pprint :refer [pprint]]))

(enable-console-print!)

(defn parse
  ([layout] (parse layout {:extended true}))
  ([layout options]
   (js/AutoLayout.VisualFormat.parse (clj->js layout) (clj->js options))))

(defn create-view
  [view-def]
  (js/AutoLayout.View. (clj->js view-def)))

(defn subview->clj
  [subview]
  {:bottom           (.-bottom subview)
   :center-x         (.-centerX subview)
   :center-y         (.-centerY subview)
   :height           (.-height subview)
   :intrinsic-height (.-intrinsicHeight subview)
   :intrinsic-width  (.-intrinsicWidth subview)
   :left             (.-left subview)
   :right            (.-right subview)
   :top              (.-top subview)
   :type             (.-type subview)
   :width            (.-width subview)
   :z-index          (.-zIndex subview)})

(defn view->clj
  [view]
  {:fitting-height (.-fittingHeight view)
   :fitting-width  (.-fittingWidth view)
   :height         (.-height view)
   :width          (.-width view)
   :subviews       (into {} (map (fn [k] [k (subview->clj (aget (.-subViews view) k))]) (.keys js/Object (.-subViews view))))})

(defn ->view
  [layout width height spacing extended?]
  (-> {:constraints (parse layout {:extended extended?})
       :width       width
       :height      height
       :spacing     spacing}
      create-view
      view->clj))

(defn vfl->view
  [layout width height spacing]
  (->view layout width height spacing false))

(defn evfl->view
  [layout width height spacing]
  (->view layout width height spacing true))

(def transform-attr (let [styles (.-style (.-documentElement js/document))
                          transform-attr (fn [name] (when (aget styles name) name))]
                      (or (transform-attr "transform")
                          (transform-attr "-webkit-transform")
                          (transform-attr "-moz-transform")
                          (transform-attr "-ms-transform")
                          (transform-attr "-o-transform"))))

(defn set-absolute-size-and-position
  [element subview]
  (set! (.-className element) (clojure.string/join " " (conj (set (filter not-empty (clojure.string/split (.-className element) #" "))) "abs")))
  (let [width (.-width subview)
        height (.-height subview)
        left (.-left subview)
        top (.-top subview)]
    (.setAttribute element "style" (str "width: " width "px; "
                                        "height: " height "px; "
                                        transform-attr ":" " translate(" left "px, " top "px);"))))

(defn build-views
  [view]
  (walk/postwalk (fn [x]
                   (if (and (vector? x) (every? string? x))
                     (let [constraints (parse x)
                           view (js/AutoLayout.View.)]
                       (.addConstraints view constraints)
                       view)
                     x))
                 view))

(defn layout-view
  [view view-element width height]
  (let [v (:layout view)]
    (.setSize v width height)
    (doseq [[id subview] (map (fn [k] [k (aget (.-subViews v) k)]) (.keys js/Object (.-subViews v)))]
      (when-not (= "_" (first id))
        (when-let [view-element (.querySelector view-element (str "#" id))]
          (set-absolute-size-and-position view-element subview)
          (when-let [sv (get-in view [:subviews (keyword id)])]
            (layout-view sv view-element (.-clientWidth view-element) #_(.-width subview) (.-clientHeight view-element) #_(.-height subview))))))))


(def vfl ["|-[child1(child3)]-[child3]-|"
          "|-[child2(child4)]-[child4]-|"
          "[child5(child4)]-|"
          "V:|-[child1(child2)]-[child2]-|"
          "V:|-[child3(child4,child5)]-[child4]-[child5]-|"])

(def evfl ["V:|-[col1:[child1(child2)]-[child2]]-|"
           "V:|-[col2:[child3(child4,child5)]-[child4]-[child5]]-|"
           "H:|-[col1(col2)]-[col2]-|"])

(rum/defc app [vfl evfl]
  [:div
   [:div {:id :left}
    [:div {:id :text} "autolajout DOM Example"]
    [:pre {:id :vfl} (clojure.string/join "\n" vfl)]
    [:div {:id :text2} "Same example written in EVFL"]
    [:pre {:id :evfl} (clojure.string/join "\n" evfl)]]
   [:div {:id :middle}
    (for [n (range 1 6)]
      [:div.child {:id (str "child" n)} [:div.child-label (str "child" n)]])]
   [:div {:id :right}
    (for [n (range 1 6)]
      [:div.child {:id (str "child" n)} [:div.child-label (str "child" n)]])]])

(rum/mount (app vfl evfl) (js/document.getElementById "app"))

(def view {:layout   ["|-[left(right,middle)]-[middle]-[right]-|"
                      "V:|-[left]-|"
                      "V:|-[middle]-|"
                      "V:|-[right]-|"]
           :subviews {:left   {:layout ["V:|-[col:[text(20)]-[vfl(evfl)]-[text2(text)]-[evfl]]-|"
                                        "|-[col]-|"]}
                      :middle {:layout evfl}
                      :right  {:layout evfl}}})

(def views (build-views view))
(layout-view views js/document (.-innerWidth js/window) (.-innerHeight js/window))

(.addEventListener js/window "resize" #(layout-view views js/document (.-innerWidth js/window) (.-innerHeight js/window)))
;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload [])
;; optionally touch your app-state to force rerendering depending on
;; your application
;; (swap! app-state update-in [:__figwheel_counter] inc)