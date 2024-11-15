(ns puzzlesnap.model
  (:require [puzzlesnap.canvas :refer [find-chunk-by-point]]
            [puzzlesnap.chunkmerge :refer [merge-chunks]]
            [puzzlesnap.utils :refer [deep-merge]]))

(defn find-chunk [db [mx my]]
  (let [canvas (js/document.getElementById "canvas-inner")
        ctx (.getContext canvas "2d")]
    (find-chunk-by-point ctx db [mx my])))

(defn rotate-chunk [db chunk-index direction]
  {:db (update-in db [:chunks chunk-index :rotation] #(mod (+ % direction) 4))
   :dispatch [:rotate-piece chunk-index]})

(defn rotate-x-y [db x y direction]
  (if-let [c (find-chunk db [x y])]
    (rotate-chunk db (:index c) direction)
    {:db db}))

(defn finish-pan [{:keys [left top pan-dx pan-dy] :as db}]
  (deep-merge db
   {:is-panning false
    :has-panned false
    :pan-start-x 0
    :pan-start-y 0
    :left (+ left pan-dx)
    :top (+ top pan-dy)
    :pan-dx 0
    :pan-dy 0}))


(defn finish-drag [db
                   drag-chunk
                   drag-chunk-dx
                   drag-chunk-dy] 
  (let
    [new-db (-> db
                (assoc :dragging nil)
                (update-in [:chunks drag-chunk :loc-x] (partial + drag-chunk-dx))
                (update-in [:chunks drag-chunk :loc-y] (partial + drag-chunk-dy))
                (merge-chunks drag-chunk))
     before (get db :piece->chunk)
     after (get new-db :piece->chunk)]
    (merge 
      {:db new-db}
      (if (not= before after) {:play-sound []} {}))))

(defn mouse-down [{:keys [chunk-order dragging] :as db}
                  x y buttons]
  (cond
    (#{2 4} buttons) {:db (deep-merge
                           db {:is-panning true
                               :pan-start-x x
                               :pan-start-y y})}
    (= buttons 3)   (when-let [{:keys [drag-chunk]} dragging]
                      (rotate-chunk db drag-chunk 1))
    :else (when-let [chunk (find-chunk db [x y])]
            {:db (deep-merge
                  db
                  {:dragging {:drag-chunk (:index chunk)
                              :drag-chunk-start-x x
                              :drag-chunk-start-y y
                              :dragged false}
                   :chunk-order (let [i (.indexOf chunk-order (:index chunk))]
                                  (vec
                                   (concat
                                    (subvec chunk-order 0 i)
                                    (subvec chunk-order (inc i) (count chunk-order))
                                    [(nth chunk-order i)])))})})))

(defn mouse-move [{:keys [is-panning pan-start-x pan-start-y scale dragging] :as db} [x y]]
  (let [{:keys [drag-chunk-start-x drag-chunk-start-y]} dragging]
    (cond
      is-panning (merge db {:pan-dx (/ (- x pan-start-x) scale)
                            :pan-dy (/ (- y pan-start-y) scale)
                            :has-panned true})
      dragging (update
                db
                :dragging
                #(merge % {:drag-chunk-dx (/ (- x drag-chunk-start-x) scale)
                           :drag-chunk-dy (/ (- y drag-chunk-start-y) scale)
                           :dragged true}))
      :else db)))


(defn mouse-up [{:keys [has-panned pan-start-x pan-start-y dragging] :as db}
                button]
  (let [{:keys [drag-chunk drag-chunk-dx drag-chunk-dy dragged]} dragging
        panned-db (finish-pan db)
        no-draggers (assoc panned-db dragging nil)]
    (cond (and drag-chunk (not dragged)) (rotate-chunk no-draggers drag-chunk (if (= button 2) 1 -1))
          (and (not= button 2) drag-chunk) (finish-drag no-draggers drag-chunk drag-chunk-dx drag-chunk-dy)
          (and (= button 2) (not has-panned) (not drag-chunk)) (rotate-x-y panned-db pan-start-x pan-start-y 1)
          :else {:db panned-db})))

