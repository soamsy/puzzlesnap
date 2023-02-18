(ns puzzlesnap.canvas 
  (:require [puzzlesnap.model :refer [piece-location]]))

(defn get-dimensions [node]
  (let [r (.getBoundingClientRect node)]
    [(.-width r) (.-height r)]))

(defn image-loaded? [image]
  (and (.-complete image) (< 0 (.-naturalHeight image))))

(defn update-canvas
  [{:keys
    [origin-x
     origin-y
     pan-dx
     pan-dy
     scale
     piece-width
     piece-height
     chunks
     drag-chunk
     drag-chunk-dx
     drag-chunk-dy] :as cv}
   canvasNode
   image]
  (when (and canvasNode image (image-loaded? image))
    (let [ctx (-> canvasNode (.getContext "2d"))
          [w h] (get-dimensions (.-parentNode canvasNode))
          draw-chunk
          (fn [{:keys [piece-grid index] :as chunk}]
            (doseq [[i j :as piece] piece-grid]
              (let [drag-dx (if (= drag-chunk index) drag-chunk-dx 0)
                    drag-dy (if (= drag-chunk index) drag-chunk-dy 0)
                    [piece-x piece-y] (piece-location cv chunk piece)
                    sx (* i piece-width)
                    sy (* j piece-height)
                    tx (+ piece-x pan-dx drag-dx)
                    ty (+ piece-y pan-dy drag-dy)]
                (-> (.drawImage
                     ctx
                     image
                     sx sy piece-width piece-height
                     tx ty piece-width piece-height)))))]
      (when (not= w (.-width canvasNode))
        (set! (.-width canvasNode) w))
      (when (not= h (.-height canvasNode))
        (set! (.-height canvasNode) h))
      (doto ctx
        (.save)
        (.clearRect 0 0 w h)
        (.scale scale scale))
      (doseq [chunk chunks] (draw-chunk chunk))
      (doto ctx
        (.restore)))))