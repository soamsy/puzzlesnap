(ns puzzlesnap.views
  (:require [day8.re-frame.http-fx]
            [puzzlesnap.canvas :refer [update-canvas]]
            [puzzlesnap.events]
            [puzzlesnap.subs]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(defn navbar []
  (r/with-let [expanded? (r/atom false)]
    [:nav.navbar.is-info>div.container
     [:div.navbar-brand
      [:a.navbar-item {:href "/" :style {:font-weight :bold}} "puzzlesnap"]
      [:span.navbar-burger.burger
       {:data-target :nav-menu
        :on-click #(swap! expanded? not)
        :class (when @expanded? :is-active)}
       [:span] [:span] [:span]]]
     [:div#nav-menu.navbar-menu
      {:class (when @expanded? :is-active)}
      [:div.navbar-start
       [:button {:on-click #(rf/dispatch [:bad-click])} "click me"]]]
     [:audio {:id "snap-sound" :src "/audio/pop.wav" :preload ""}]]))

(defn offset-coords [canvas x y]
   [(- x (.-offsetLeft canvas))
    (- y (.-offsetTop canvas))])

(defn on-mouse-down [[x y] pan?]
  (rf/dispatch [:mouse-down x y pan?]))

(defn on-mouse-move [[x y]]
  (rf/dispatch [:mouse-move x y]))

(defn on-mouse-up []
  (rf/dispatch [:mouse-up]))

(defn on-mouse-out [x y]
  (on-mouse-up))

(defn on-mouse-wheel [x y deltaY]
  (rf/dispatch [:mouse-wheel x y deltaY]))

(defn init-canvas [canvas]
  (let [getTouch (fn [e] (aget (.-touches e) 0))]
    (doto canvas
      (.addEventListener "mousedown" #(on-mouse-down (offset-coords canvas (.-x %) (.-y %)) (#{2 4} (.-buttons %))))
      (.addEventListener "mousemove" #(on-mouse-move (offset-coords canvas (.-x %) (.-y %))))
      (.addEventListener "touchstart" #(on-mouse-down
                                        (offset-coords
                                         canvas
                                         (-> % getTouch .-clientX)
                                         (-> % getTouch .-clientY))
                                        true) #js {:passive true})
      (.addEventListener "touchmove" #(on-mouse-move
                                       (offset-coords
                                        canvas
                                        (-> % getTouch .-clientX)
                                        (-> % getTouch .-clientY))) #js {:passive true})
      (.addEventListener "mouseup" #(on-mouse-up))
      (.addEventListener "touchend" #(on-mouse-up))
      (.addEventListener "mouseout" #(on-mouse-out (.-pageX %) (.-pageY %)))
      (.addEventListener "mousewheel" #(on-mouse-wheel (.-clientX %) (.-clientY %) (.-deltaY %))))))

(defn canvas-inner [img-ref]
  (let [canvas-ref (atom nil)
        db (rf/subscribe [:db])]
    (r/create-class
     {:reagent-render
      (fn []
        @db
        [:canvas.canvas-inner {:ref #(reset! canvas-ref %)}])
      :component-did-mount
      (fn []
        (init-canvas @canvas-ref))
      :component-did-update
      (fn []
        (update-canvas @db @canvas-ref @img-ref))
      :display-name "canvas-inner"})))

(defn canvas-outer []
  (r/with-let
    [image-uri (rf/subscribe [:global/image-uri])
     resize-handler #(rf/dispatch [:local-tick])
     _ (.addEventListener js/window "resize" resize-handler)
     no-context-menu #(.preventDefault %)
     _ (.addEventListener js/document "contextmenu" no-context-menu)]
    (let [img-ref (atom nil)]
      [:section.canvas-outer
       [(canvas-inner img-ref)]
       [:img {:ref #(reset! img-ref %)
              :id "invisible-image"
              :src @image-uri
              :style {:display "none"}
              :on-load #(rf/dispatch [:image-loaded @img-ref])}]])
    (finally (.removeEventListener js/window "resize" resize-handler)
             (.removeEventListener js/document "contextmenu" no-context-menu))))

(defn page []
  [:div.full
   [navbar]
   [canvas-outer]])
