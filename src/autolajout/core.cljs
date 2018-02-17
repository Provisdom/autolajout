(ns autolajout.core
    (:require cljsjs.autolayout
              [rum.core :as rum]
              [cljs.pprint :refer [pprint]]))

(enable-console-print!)

(defn parse
  ([layout] (parse layout {:extended false}))
  ([layout options]
   (js/AutoLayout.VisualFormat.parse (clj->js layout) (clj->js options))))

(defn create-view
  [view-def]
  (js/AutoLayout.View. (clj->js view-def)))

(defn subview->clj
  [subview]
  {:bottom (.-bottom subview)
   :center-x (.-centerX subview)
   :center-y (.-centerY subview)
   :height (.-height subview)
   :intrinsic-height (.-intrinsicHeight subview)
   :intrinsic-width (.-intrinsicWidth subview)
   :left (.-left subview)
   :right (.-right subview)
   :top (.-top subview)
   :type (.-type subview)
   :width (.-width subview)
   :z-index (.-zIndex subview)})

(defn view->clj
  [view]
  {:fitting-height (.-fittingHeight view)
   :fitting-width (.-fittingWidth view)
   :height (.-height view)
   :width (.-width view)
   :subviews (into {} (map (fn [k] [(keyword k) (subview->clj (aget (.-subViews view) k))]) (.keys js/Object (.-subViews view))))})

(defn ->view
  [layout width height spacing extended?]
  (-> {:constraints (parse layout {:extended extended?})
       :width width
       :height height
       :spacing spacing}
      create-view
      view->clj))

(defn vfl->view
  [layout width height spacing]
  (->view layout width height spacing false))

(defn evfl->view
  [layout width height spacing]
  (->view layout width height spacing true))

(def tranform-attr (let [styles (.-style (.-documentElement js/document))
                         transform-attr (fn [name] (when (aget styles name) name))]
                     (or (transform-attr "transform")
                         (transform-attr "-webkit-transform")
                         (transform-attr "-moz-transform")
                         (transform-attr "-ms-transform")
                         (transform-attr "-o-transform"))))

(defn set-absolute-size-and-position
  [element {:keys [width height left top] :as subview}]
  (.setAttribute element "style" (str "width: " width "px; "
                                      "height: " height "px; "
                                      tranform-attr ":" " translate3d(" left "px, " top "px, 0px);")))

(defn auto-layout
  [layout parent-id]
  (let [parent-element (.getElementById js/document parent-id)
        v (js/AutoLayout.View.)
        constraints (parse layout {:extended true})
        _ (.addConstraints v constraints)
        view (view->clj v)
        elements (into {}
                       (for [[id subview] (:subviews view)]
                         (let [element (.getElementById js/document (name id))]
                           (when element
                             (set! (.-className element) (if (.-className element) (str (.-className element) " abs") "abs"))
                             [id element]))))
        update-layout (fn []
                        (let [width (if parent-element (.-clientWidth parent-element) (.-innerWidth js/window))
                              height (if parent-element (.-clientHeight parent-element) (.-innerHeight js/window))
                              _ (.setSize v width height)
                              view (view->clj v)]
                          (doseq [[id element] elements]
                            (let [subview (-> view :subviews id)]
                              (set-absolute-size-and-position element subview)))))]
    (.addEventListener js/window "resize" update-layout)
    (update-layout)
    update-layout))

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
   [:div {:id "left"}
    [:div {:id "text"} "autolajout DOM Example"]
    [:pre {:id "vfl"} (clojure.string/join "\n" vfl)]
    [:div {:id "text2"} "Same example written in EVFL"]
    [:pre {:id "evfl"} (clojure.string/join "\n" evfl)]]
   [:div {:id "right"}
    [:div {:id "child1"} [:div "child1"]]
    [:div {:id "child2"} [:div "child2"]]
    [:div {:id "child3"} [:div "child3"]]
    [:div {:id "child4"} [:div "child4"]]
    [:div {:id "child5"} [:div "child5"]]]])

(rum/mount (app vfl evfl) (js/document.getElementById "app"))
(auto-layout ["|-[left(right)]-[right]-|"
              "V:|-[left]-|"
              "V:|-[right]-|"]
             "app")
(auto-layout ["V:|-[col:[text(20)]-[vfl(evfl)]-[text2(text)]-[evfl]]-|"
              "|-[col]-|"]
             "left")
(auto-layout evfl "right")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)