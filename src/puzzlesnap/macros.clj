(ns puzzlesnap.macros)

(defmacro save-restore [ctx & body]
  `(do
     (.save ~ctx)
     (let [result# (do ~@body)]
       (.restore ~ctx)
       result#)))