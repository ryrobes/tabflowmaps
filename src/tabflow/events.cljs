(ns tabflow.events
  (:require
   [re-frame.core :as re-frame]
   [tabflow.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced]]))

(re-frame/reg-event-db
 ::initialize-db
 #_{:clj-kondo/ignore [:unresolved-symbol]}
 (fn-traced [_ _]
   db/default-db))

;; tiny project, just left them all in views.cljs
