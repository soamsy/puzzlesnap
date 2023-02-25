(ns puzzlesnap.canvas
  (:require [puzzlesnap.model :refer [piece-location]]))

(defn get-dimensions [node]
  (let [r (.getBoundingClientRect node)]
    [(.-width r) (.-height r)]))

(defn image-loaded? [image]
  (and (.-complete image) (< 0 (.-naturalHeight image))))

(defn bezier-to [ctx ref-x ref-y cx1 cy1 cx2 cy2 x y]
  (.bezierCurveTo ctx
                  (+ ref-x cx1)
                  (+ ref-y cy1)
                  (+ ref-x cx2)
                  (+ ref-y cy2)
                  (+ ref-x x)
                  (+ ref-y y)))

(defn bezier-normal  [ctx ref-x ref-y {:keys [cx1 cy1]} {:keys [x y cx2 cy2]}]
  (bezier-to ctx ref-x ref-y cx1 cy1 cx2 cy2 x y))

(defn bezier-reverse [ctx ref-x ref-y {:keys [cx2 cy2]} {:keys [x y cx1 cy1]}]
  (bezier-to ctx ref-x ref-y cx2 cy2 cx1 cy1 x y))

(defn draw-piece
  [ctx image
   {piece-width :piece-width
    piece-height :piece-height
    {:keys [left-right top-bottom]} :tabs :as cv}
    chunk
   [i j :as piece]
   extra-dx extra-dy]
  (let [[piece-x piece-y] (piece-location cv chunk piece)
        [sx sy] [(* i piece-width) (* j piece-height)]
        [tx ty] [(+ piece-x extra-dx) (+ piece-y extra-dy)]
        [bx by] [(/ piece-width 2) (/ piece-height 2)]
        top (some-> top-bottom (get i) (get j))
        [ix iy] [(get-in top [0 :x]) (get-in top [0 :y])]
        bottom (some-> top-bottom (get i) (get (inc j)))
        left (some-> left-right (get i) (get j))
        right (some-> left-right (get (inc i)) (get j))]
    (doto ctx (.beginPath))
    (doto ctx (.moveTo (+ tx ix) (+ ty iy)))
    (doseq [[prev curr] (partition 2 1 top)]
      (bezier-normal ctx tx ty prev curr))

    (doseq [[prev curr] (partition 2 1 right)]
      (bezier-normal ctx (+ tx piece-width) ty prev curr))

    (doseq [[prev curr] (partition 2 1 (reverse bottom))]
      (bezier-reverse ctx tx (+ ty piece-height) prev curr))

    (doseq [[prev curr] (partition 2 1 (reverse left))]
      (bezier-reverse ctx tx ty prev curr))
    (.stroke ctx)
    (doto ctx
      (.save)
      (.clip)
      (.drawImage
       image
       (- sx bx) (- sy by) (+ piece-width (* 2 bx)) (+ piece-height (* 2 by))
       (- tx bx) (- ty by) (+ piece-width (* 2 bx)) (+ piece-height (* 2 by)))
      (.restore))))

(defn draw-chunk
  [ctx image
   {:keys [drag-chunk
           drag-chunk-dx drag-chunk-dy
           pan-dx pan-dy] :as cv}
   {:keys [piece-grid index] :as chunk}]
  (let [[drag-dx drag-dy] (if (= drag-chunk index)
                            [drag-chunk-dx drag-chunk-dy]
                            [0 0])]
    (doseq [piece piece-grid]
      (draw-piece ctx image cv chunk piece (+ pan-dx drag-dx) (+ pan-dy drag-dy)))))

(defn update-canvas
  [{:keys [left top scale chunks chunk-order] :as cv}
   canvasNode
   image]
  (when (and canvasNode image (image-loaded? image))
    (let [ctx (-> canvasNode (.getContext "2d"))
          [w h] (get-dimensions (.-parentNode canvasNode))]
      (when (not= w (.-width canvasNode))
        (set! (.-width canvasNode) w))
      (when (not= h (.-height canvasNode))
        (set! (.-height canvasNode) h)) 
      (set! (.-lineJoin ctx) "round")
      (set! (.-lineWidth ctx) 0.01)
      (doto ctx
        (.save)
        (.clearRect 0 0 w h)
        (.scale scale scale)
        (.translate left top))
      (doseq [i chunk-order]
        (draw-chunk ctx image cv (get chunks i)))
      (doto ctx
        (.restore)))))