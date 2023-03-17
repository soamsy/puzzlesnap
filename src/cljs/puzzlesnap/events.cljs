(ns puzzlesnap.events
  (:require [puzzlesnap.db :refer [default-db]]
            [puzzlesnap.model :refer [mouse-down mouse-move mouse-up]]
            [puzzlesnap.initroom :refer [init-puzzle]]
            [re-frame.core :refer [debug path reg-event-db reg-event-fx reg-fx trim-v]]))

(reg-event-fx
 :app/initialize
 (fn [_ _]
   {:db default-db}))

(def update-interceptor [ trim-v])

(def global-interceptor [debug (path :global) trim-v])
(def shared-interceptor [debug (path :shared) trim-v])
(def local-interceptor [ (path :local) trim-v])

(reg-event-db
 :local-tick
 local-interceptor
 (fn [ldb _] (update ldb :id inc)))

(defn init-image-state [ldb puzzle-width puzzle-height img]
  (-> ldb
      (assoc :image-loaded true)
      (assoc :image-width (.-naturalWidth img))
      (assoc :image-height (.-naturalHeight img))
      (assoc :piece-width (/ (.-naturalWidth img) puzzle-width))
      (assoc :piece-height (/ (.-naturalHeight img) puzzle-height))
      (assoc :scale (* 0.8
                       (js/Math.min
                        (/ js/innerWidth (.-naturalWidth img))
                        (/ js/innerHeight (.-naturalHeight img)))))))

(reg-event-db
 :image-loaded
 update-interceptor
 (fn [{{:keys [image-loaded] :as ldb} :local
       {:keys [puzzle-width puzzle-height]} :global :as db} [img]]
 (if image-loaded
   (update-in db [:local :id] inc)
   (-> db
       (update :local #(init-image-state % puzzle-width puzzle-height img))
       (init-puzzle)))))

(reg-event-fx
 :mouse-down
 update-interceptor
 (fn [{:keys [db]} [x y buttons]]
   (mouse-down db x y buttons)))

(reg-event-db
 :mouse-move
 (remove #{debug} update-interceptor)
 mouse-move)

(reg-event-fx
 :mouse-up
 update-interceptor
 (fn [{:keys [db]} [button]]
   (mouse-up db button)))

(reg-fx
 :play-sound
 (fn [_]
   (let [el (-> js/document (.getElementById "snap-sound"))]
     (set! (.-volume el) 0.7)
     (.play el))))

(reg-event-db
 :mouse-wheel
 local-interceptor
 (fn [{:keys [scale left top] :as ldb} [x y deltaY]]
   (let [scale-factor (if (< 0 deltaY) 0.85 1.15)
         new-scale (js/Math.min 1.5 (js/Math.max 0.4 (* scale scale-factor)))
         ratio (/ new-scale scale)
         new-x (* x ratio)
         new-y (* y ratio)]
     (merge
      ldb
      {:scale new-scale
       :left (+ left (/ (- x new-x) new-scale))
       :top (+ top (/ (- y new-y) new-scale))}))))

(reg-event-fx
 :rotate-piece
 update-interceptor
 (fn [{:keys [db]} [chunk-index]]
   (let [chunk (get-in db [:shared :chunks chunk-index])
         angle (get-in db [:local :rotations chunk-index])
         target-angle (* (/ js/Math.PI 2) (get chunk :rotation))
         diff (mod (- target-angle angle) (* 2 js/Math.PI))
         tick (/ js/Math.PI 16)
         should-stop (< diff tick)]
     (if should-stop
       {:db (assoc-in db [:local :rotations chunk-index] target-angle)}
       {:db (assoc-in db [:local :rotations chunk-index] (+ angle (if (< (+ js/Math.PI 0.001) diff) (- tick) tick)))
        :dispatch-later {:ms 12 :dispatch [:rotate-piece chunk-index]}}))))