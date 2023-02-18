(ns puzzlesnap.app
  (:require [puzzlesnap.init :refer [init!]]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(init!)
