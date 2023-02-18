(ns puzzlesnap.init
  (:require [day8.re-frame.http-fx]
            [puzzlesnap.ajax :as ajax]
            [puzzlesnap.events]
            [puzzlesnap.subs]
            [puzzlesnap.views :refer [page]]
            [re-frame.core :as rf]
            [reagent.dom :as rdom]))

(defn ^:dev/after-load mount-components []
  (rf/clear-subscription-cache!)
  (rdom/render [#'page] (.getElementById js/document "app")))

(defn init! []
  (rf/dispatch [:app/initialize])
  (ajax/load-interceptors!)
  (mount-components))
