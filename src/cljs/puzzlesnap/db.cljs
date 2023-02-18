(ns puzzlesnap.db)


(def default-db
  {:canvas
   {:draw-size 100
    :left 100
    :top 100
    :is-panning false
    :pan-start-x 0
    :pan-start-y 0
    :pan-dx 0
    :pan-dy 0
    :scale 0.5
    :image-uri "/img/puzzle.jpg"
    :image-loaded false
    :image-width 0
    :image-height 0
    :puzzle-width 5
    :puzzle-height 3
    :drag-chunk nil
    :drag-chunk-start-x 0
    :drag-chunk-start-y 0
    :drag-chunk-dx 0
    :drag-chunk-dy 0}})