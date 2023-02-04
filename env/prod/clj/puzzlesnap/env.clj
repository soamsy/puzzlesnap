(ns puzzlesnap.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[puzzlesnap started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[puzzlesnap has shut down successfully]=-"))
   :middleware identity})
