(ns puzzlesnap.canvas
  (:require [puzzlesnap.grid :refer [chunk-center piece-loc]])

  (:require-macros [puzzlesnap.macros :refer [save-restore]]))

(defn get-dimensions [node]
  (let [r (.getBoundingClientRect node)]
    [(.-width r) (.-height r)]))

(defn image-loaded? [image]
  (and (.-complete image) (< 0 (.-naturalHeight image))))

(defn blur-on [ctx drag]
  (set! (.-shadowBlur ctx) 10)
  (set! (.-shadowColor ctx) (if drag "black" "rgba(0,0,0,0.3)"))
  (set! (.-shadowOffsetX ctx) (if drag 2 2))
  (set! (.-shadowOffsetY ctx) (if drag 2 2)))

(defn blur-off [ctx]
  (set! (.-shadowBlur ctx) 0.0)
  (set! (.-shadowColor ctx) "black")
  (set! (.-shadowOffsetX ctx) 0)
  (set! (.-shadowOffsetY ctx) 0))

(defn default-styles [ctx piece-length]
  (set! (.-lineWidth ctx) (max 1 (js/Math.round (/ piece-length 100))))
  (set! (.-strokeStyle ctx) "black")
  (set! (.-fillStyle ctx) "black")
  (blur-off ctx))

(defn reposition-canvas
  [ctx {:keys [left top pan-dx pan-dy scale]}]
  (doto ctx
    (.scale scale scale)
    (.translate (+ left pan-dx) (+ top pan-dy))))

(defn reposition-to-chunk
  [ctx 
   {:keys [rotations] :as db}
   dragging
   {:keys [index loc-x loc-y] :as chunk}]
  (let [drag-dx (get dragging :drag-chunk-dx 0)
        drag-dy (get dragging :drag-chunk-dy 0)
        [cx cy] (chunk-center db chunk)]
    (.translate ctx (+ loc-x cx drag-dx) (+ loc-y cy drag-dy))
    (.rotate ctx (get rotations index))
    (.translate ctx (- 0 cx) (- 0 cy))))

(defn reposition-to-piece
  [ctx db chunk piece]
  (let [[x y] (piece-loc db chunk piece)]
    (.translate ctx x y)))

(defn draw-shadow
  [ctx
   {:keys [paths] :as db}
   chunk
   piece]
   (let [path (get paths piece)]
     (save-restore
      ctx
      (doto ctx
        (reposition-to-piece db chunk piece)
        (.fill path)
        (.stroke path)))))

(defn draw-piece
  [ctx image
   {:keys [piece-width piece-height paths] :as db}
   chunk
   [i j :as piece]]
  (let [[sx sy] [(* i piece-width) (* j piece-height)]
        [bx by] [(/ piece-width 2) (/ piece-height 2)]
        path (get paths piece)]
    (save-restore
     ctx
     (doto ctx
       (reposition-to-piece db chunk piece)
       (.clip path)
       (.drawImage
        image
        (- sx bx) (- sy by) (+ piece-width (* 2 bx)) (+ piece-height (* 2 by))
        (- bx) (- by) (+ piece-width (* 2 bx)) (+ piece-height (* 2 by)))
       (.stroke path)))))

(defn draw-chunk
  [ctx
   image
   {:keys [dragging] :as db}
   {:keys [piece-grid] :as chunk}]
  (when (seq piece-grid)
    (let [drag (if (= (:drag-chunk dragging) (:index chunk)) dragging nil)]
      (save-restore
       ctx
       (reposition-to-chunk ctx db drag chunk)
       (blur-on ctx drag)
       (doall
        (for [piece piece-grid]
          (draw-shadow ctx db chunk piece)))
       (blur-off ctx)
       (doall
        (for [piece piece-grid]
          (draw-piece ctx image db chunk piece)))))))

(defn draw-chunks
  [ctx
   image
   {:keys [chunks chunk-order] :as db}]
  (save-restore
   ctx
   (reposition-canvas ctx db)
   (doall
    (for [i chunk-order]
      (let [chunk (get chunks i)]
        (draw-chunk ctx image db chunk))))))

(defn find-chunk-by-point
  [ctx
   {:keys [paths chunks chunk-order dragging] :as db}
   [x y]]
  (save-restore
   ctx
   (reposition-canvas ctx db)
   (->>
    (for [i chunk-order]
      (let [{:keys [piece-grid] :as chunk} (get chunks i)]
        (save-restore
         ctx
         (reposition-to-chunk ctx db dragging chunk)
         (first
          (filter
           seq
           (for [piece piece-grid]
             (save-restore
              ctx
              (reposition-to-piece ctx db chunk piece)
              (when (.isPointInPath ctx (get paths piece) x y)
                chunk))))))))
    (filter seq)
    (first))))

(defn update-canvas
  [db
   canvasNode
   image]
  (when (and canvasNode image (image-loaded? image))
    (let [ctx (-> canvasNode (.getContext "2d"))
          [w h] (get-dimensions (.-parentNode canvasNode))]
      (when-not (= w (.-width canvasNode))
        (set! (.-width canvasNode) w))
      (when-not (= h (.-height canvasNode))
        (set! (.-height canvasNode) h))
      (set! (.-lineJoin ctx) "round")
      (default-styles ctx (get db :piece-width))
      (.clearRect ctx 0 0 w h)
      (draw-chunks ctx image db))))