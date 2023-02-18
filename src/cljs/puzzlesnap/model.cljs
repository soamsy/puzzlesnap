(ns puzzlesnap.model)

(defn piece-location [{:keys [origin-x origin-y piece-width piece-height] :as cv}
                      {:keys [loc-x loc-y main-piece-x main-piece-y] :as chunk}
                      [px py]]
  [(+ origin-x loc-x (* piece-width (- px main-piece-x)))
   (+ origin-y loc-y (* piece-height (- py main-piece-y)))])