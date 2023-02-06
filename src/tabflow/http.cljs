(ns tabflow.http
  (:require
   [re-frame.core :as re-frame]
   ;[ajax.core :as ajax]
   [ajax.edn :as ajax-edn]
   ;[cljs.tools.reader :refer [read-string]]
   [clojure.string :as cstr]
   [tabflow.config :as config]
   [day8.re-frame.http-fx]))

(def server-http-port 8888) ;; hardcoded for dev
(def url-base (cond config/debug? ;; in dev mode fe and be have diff ports 
                    (str (cstr/join ":" (drop-last (cstr/split
                                                    (.. js/document -location -href) #":"))) ":" server-http-port)
                    :else (let [b (str (.. js/document -location -href))]
                            (if (cstr/ends-with? b "/") (cstr/join "" (drop-last b)) b))))

(defn read-file [file callback] ;; annoying because FileReader is async
  (let [js-file-reader (js/FileReader.)]
    (set! (.-onload js-file-reader)
          (fn [evt]
            (let [result (-> evt .-target .-result)]
              (callback result))))
    (.readAsText js-file-reader file)))

(re-frame/reg-event-db
 ::failure-http-get-twb
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :get-twb])]
     (assoc-in db [:http-reqs :get-twb]
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-http-get-twb
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :get-twb])]
     (-> db 
         (assoc :twb-root result)
         (assoc-in [:http-reqs :get-twb]
               (merge old-status
                      {:keys (count result)
                       :ended-unix (.getTime (js/Date.))
                       :status "success"}))))))

(re-frame/reg-event-fx
 ::load ;; load a pre-processed TWB->EDN file from webroot, not from REST server.
 (fn [{:keys [db]} [_]]
   (let [method :get
         url "/US_Superstore_14.twb.edn"]
     (js/console.log url-base)
     {:db   (assoc-in db [:http-reqs :get-twb]
                      {:status "running"
                       :url url
                       :start-unix (.getTime (js/Date.))})
      :http-xhrio {:method          method
                   :uri             url
                   ;:params          request
                   :timeout         28000
                   :format          (ajax-edn/edn-request-format)
                   :response-format (ajax-edn/edn-response-format)
                   :on-success      [::success-http-get-twb]
                   :on-failure      [::failure-http-get-twb]}})))


(re-frame/reg-event-db
 ::failure-http-proc-twb
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :proc-twb])]
     (assoc-in db [:http-reqs :proc-twb] ; comp key from ::get-http-data
               (merge old-status
                      {:status "failed"
                       :ended-unix (.getTime (js/Date.))
                       :message result})))))

(re-frame/reg-event-db
 ::success-http-proc-twb
 (fn [db [_ result]]
   (let [old-status (get-in db [:http-reqs :proc-twb])]
     (-> db
         (assoc :twb-root (get result :image))
         (assoc :twb-file (get result :fname))
         (assoc :selected-dash nil)
         (assoc :selected nil)
         (assoc-in [:http-reqs :proc-twb]
                   (merge old-status
                          {:keys (count result)
                           :ended-unix (.getTime (js/Date.))
                           :status "success"}))))))

(re-frame/reg-event-fx
 ::proc-twb ;; send uploaded file data to the REST server to send back modded EDN
 (fn [{:keys [db]} [_ fname fdata]]
   (when (cstr/ends-with? (cstr/lower-case fname) ".twb")
     (let [method :post
           url (str url-base "/load-twb")
           request {:image fdata
                    :fname fname}]
       (tap> [(str url-base "/load-twb")])
       {:db   (assoc-in db [:http-reqs :proc-twb]
                        {:status "running"
                         :url url
                         :start-unix (.getTime (js/Date.))})
        :http-xhrio {:method          method
                     :uri             url
                     :params          request
                     :timeout         28000
                     :format          (ajax-edn/edn-request-format)
                     :response-format (ajax-edn/edn-response-format)
                     :on-success      [::success-http-proc-twb]
                     :on-failure      [::failure-http-proc-twb]}}))))