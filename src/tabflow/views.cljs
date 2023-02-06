(ns tabflow.views
  (:require
   [reagent.core :as reagent :refer [atom]]
   [re-frame.core :as re-frame]
   [re-com.core :as re-com :refer [at]]
   [re-com.util :refer [px]]
   [breaking-point.core :as bp]
   ;[re-pressed.core :as rp]
   ;[tabflow.events :as events]
   [clojure.string :as cstr]
   [tabflow.db :as db]
   [tabflow.util :as ut :refer [keypaths unkeyword deep-flatten curved-path-h]]
   [tabflow.http :as http]
   [clojure.set :as cset]
   ;[tabflow.subs :as subs]
   [cljs-drag-n-drop.core :as dnd2]
   ["react-colorful" :as react-colorful]
   ["react-zoom-pan-pinch" :as zpan]))

(defonce action-scheme (atom :Paired))
(defonce filter-scheme (atom :RdPu))
(defonce param-scheme (atom :YlGnBu))
(defonce basic-block-color (atom "#D8BFD8"))
(defonce options? (atom false))
(defonce hello? (atom true))

(re-frame/reg-sub
 ::dashboards
 (fn [db] (get-in db [:twb-root :dashboards])))

(re-frame/reg-sub
 ::actions
 (fn [db] (get-in db [:twb-root :actions])))

(re-frame/reg-sub
 ::file-name
 (fn [db] (get db :twb-file)))

(re-frame/reg-sub
 ::req-running?
 (fn [db] (let [h1 (get-in db [:http-reqs :get-twb :status])
                h2 (get-in db [:http-reqs :proc-twb :status])]
            (or (= "running" h1) (= "running" h2)))))

(re-frame/reg-sub
 ::worksheets
 (fn [db] (get-in db [:twb-root :worksheets])))

(re-frame/reg-sub
 ::worksheet-parameters
 (fn [db] (let [members (doall (vec (apply concat (for [k (get-in db [:twb-root :dashboards])]
                                                    (remove nil? (into []
                                                                       (for [v (get-in db [:twb-root :dashboards (key k) :datasource-dependencies])]
                                                                         (when (not (empty? (select-keys v [:column-attrs])))
                                                                           (merge v {:dashboard (key k)})))))))))
                mm (into {} (for [e members
                                  :when (not (nil? (get-in e [:column-attrs :name])))]
                              {[(get-in e [:column-attrs :name]) (get e :dashboard)]
                               (merge (get-in e [:column-attrs])
                                      {:dashboard (get e :dashboard)})}))
                action-params (into {} (remove #(empty? (last %))
                                               (into {} (for [k (get-in db [:twb-root :actions])]
                                                          {(key k) (into {} (apply merge
                                                                                   (for [v (get-in db [:twb-root :actions (key k) :params])
                                                                                         :when (not (empty? (select-keys v [:param-attrs])))]
                                                                                     (merge {:param-domain-type (get-in db [:twb-root :actions (key k) :ttype])}
                                                                                            (get-in db [:twb-root :actions (key k) :source-attrs])
                                                                                            (dissoc (merge v ;{:dashboard (key k)}
                                                                                                           {(keyword (get-in v [:param-attrs :name]))
                                                                                                            (get-in v [:param-attrs :value])}) :param-attrs)))))}))))
                ma (into {} (for [[k v] action-params]
                              {[(if (= (get v :param-domain-type) :nav-action)
                                  k
                                  (cstr/replace (str (get v :target-parameter)) "[Parameters]." ""))
                                (get v :dashboard)]
                               (merge v {:from-action k
                                         :source-field2 (if (not (= (get v :param-domain-type) :nav-action))
                                                          (try (str "[" (nth (cstr/split (get v :source-field) #":") 1) "]")
                                                               (catch :default _ nil)))
                                         :caption "(inferred via param action)"})}))]
            (merge mm ma))))


(re-frame/reg-sub
 ::objects-in-dashboard
 (fn [db [_ dashboard-name type]]
   (let [base (into {} (distinct (for [e (filter #(some (fn [x] (= x :name)) %) (keypaths (get-in db [:twb-root :dashboards dashboard-name])))]
     {(vec (apply merge [:twb-root :dashboards dashboard-name] e)) 
      (get-in db (vec (apply merge [:twb-root :dashboards dashboard-name] e)))}
                        ;(get-in db (vec (apply merge [:twb-root :dashboards dashboard-name] e)))
                        )))
         type-filter (cond (= type :worksheets) (vec (distinct (vals (filter #(cstr/includes? (str (key %)) ":zone") base))))
                           (= type :fields) (vec (distinct (vals (filter #(not (cstr/includes? (str (key %)) ":zone")) base))))
                           :else base)]
     type-filter)))

(re-frame/reg-sub
 ::target-worksheets
 (fn [db [_ name]] ;; optional action name 
   (let [members (into {}
                       (for [k (get-in db [:twb-root :actions])]
                         (into {}
                               (when (if (nil? name) true
                                         (= (key k) name))
                                 {(key k)
                                  (into {} (for [m (get-in db [:twb-root :actions (key k) :command])]
                                             {(keyword (get-in m [:param-attrs :name]))
                                              (get-in m [:param-attrs :value])}))}))))
         mem (into {} (for [[k v] members
                            :let [dash (get v :target)
                                  exclude (try (vec (remove empty? (for [e (cstr/split (get v :exclude) #",")] (cstr/trim e)))) (catch :default _ []))
                                  fields (try (vec (remove empty? (for [e (cstr/split (get v :field-captions) #",")] (cstr/trim e)))) (catch :default _ []))
                                  poss-targets @(re-frame/subscribe [::objects-in-dashboard dash :worksheets])
                                  sources [(get-in db [:twb-root :actions k :source-attrs :worksheet])]
                                  esources (vec (for [e (get-in db [:twb-root :actions k :source])]
                                                  (get-in e [:exclude-sheet-attrs :name])))
                                  comp-sources (vec (cset/difference (set poss-targets) (set esources)))]]
                        {k (merge v {:worksheets (vec (cset/difference (set poss-targets) (set exclude)))
                                     :targets (vec (cset/difference (set poss-targets) (set exclude)))
                                     :target-exclude exclude
                                     :dashboard (get-in db [:twb-root :actions k :source-attrs :dashboard])
                                     :sources (if (empty? comp-sources) sources comp-sources)
                                     :source-exclude esources
                                     :fields (if (nil? fields) [] fields)})}))]
     (dissoc mem nil))))

(re-frame/reg-event-db
 ::select
 (fn [db [_ tid]]
   (assoc db :selected tid)))

(re-frame/reg-sub
 ::selected
 (fn [db] (get db :selected)))

(re-frame/reg-event-db
 ::select-dash
 (fn [db [_ dash-name]]
   (assoc db :selected-dash dash-name)))

(re-frame/reg-sub
 ::selected-dash
 (fn [db] 
   (let [selected-dash (get db :selected-dash)
         dashboards (keys @(re-frame/subscribe [::dashboards]))
         selected-dash (if (nil? selected-dash) (first dashboards) selected-dash)]
     selected-dash)))


(re-frame/reg-sub
 ::position-map 
 (fn [db [_ selected-dash]] 
   (let [selected @(re-frame/subscribe [::selected]) ;; TODO, unpack subs in subs issue - was in hackathon mode 
         worksheets @(re-frame/subscribe [::worksheets]) ;; TODO, unpack subs in subs issue, etc 
         worksheet-filter-targets (into {}
                                        (for [[k v] worksheets]
                                          {k (vec (distinct (for [kp (filter #(and (cstr/includes? (str %) ":datasource-dependencies")
                                                                                   (= :column (last %))) (keypaths v))]
                                                              (get-in v kp))))}))
         target-worksheets @(re-frame/subscribe [::target-worksheets nil])
         source-worksheets (vec (distinct (remove nil? (apply concat (for [[_ v] target-worksheets
                                                                           :when (= (get v :dashboard) selected-dash)]
                                                                       (get v :sources))))))
         target-worksheets-all (vec (distinct (apply concat (remove nil? (for [[_ v] target-worksheets] (get v :worksheets))))))
         dash-sheets @(re-frame/subscribe [::objects-in-dashboard selected-dash :worksheets])
         actions @(re-frame/subscribe [::actions])
         worksheet-filters @(re-frame/subscribe [::worksheet-parameters])
         actions-involved (keys (into {} (filter #(some (fn [x] (= x (last selected))) (get (last %) :sources)) target-worksheets)))
         target-worksheets-selected (vec (flatten (for [[_ v] (into {} (filter #(some (fn [x] (= x (last selected))) (get (last %) :targets)) target-worksheets))]
                                                    (get v :targets))))
         wk-actions-wmap (fn [w] (into {}
                                       (for [[k v] (into {}
                                                         (filter #(some (fn [x] (= x w))
                                                                        (get (last %) :sources)) target-worksheets))]
                                         {k (if (empty? (get v :fields)) ["(all fields)"] (get v :fields))})))

         ;;; actions
         actions (filter #(= selected-dash (get-in (last %) [:source-attrs :dashboard])) actions)
         action-keys (remove nil? (keys actions))
         pos-action-blocks {:action (into {} (for [b (range (count action-keys))]
                                               (let [x 500
                                                     y (+ 10 (* b 75))
                                                     action-key (nth action-keys b)
                                                     action-name (str action-key)
                                                     related-action-selected? (some #(= % action-name) actions-involved)
                                                     fieldsets-map {:filter-left
                                                                    {:x (+ 0 x) ;; 300 is block width
                                                                     :y (+ 35 y)}
                                                                    :filter-right
                                                                    {:x (+ 360 x) ;; 300 is block width
                                                                     :y (+ 35 y)}}
                                                     flags (-> []
                                                               (conj (if related-action-selected? :related-action-selected nil)))
                                                     command (get-in (into {} actions) [action-key :command-attrs :command]
                                                                     (get-in (into {} actions) [action-key :ttype]))]
                                                 ;(tap> [:foo (into {} actions)])
                                                 {action-name
                                                  {:type :action
                                                   :id b :x x :y y
                                                   :color (let [scm @action-scheme ; :YlGnBu ; :Pastel2 ;:RdPu ;:PuRd
                                                                the-keys (count action-keys)
                                                                ks (vec (filter integer? (for [n (keys (get db/colorbrewer scm))] (js/parseInt (unkeyword n)))))
                                                                mnks (apply min ks)
                                                                mxks (apply max ks)
                                                                ckey (cond (< the-keys mnks) mnks
                                                                           (> the-keys mxks) mxks
                                                                           :else the-keys)
                                                                cnts (keyword (str ckey))]
                                                            (get-in db/colorbrewer [scm cnts b]))
                                                   :fieldsets nil
                                                   :fieldsets-map fieldsets-map
                                                   :action-key action-key
                                                   :name action-name
                                                   :flags flags
                                                   :command command}})))}

        ;;;; source-worksheets
         worksheets (select-keys worksheets dash-sheets)
         worksheet-keys (remove nil? (keys worksheets))
         cnt-wks (count worksheet-keys)
         actions-all2 (into {}
                            (for [b (range cnt-wks)]
                              {b (keys (into {}
                                             (filter #(some (fn [x] (= x (str (nth worksheet-keys b))))
                                                            (get (last %) :sources)) target-worksheets)))}))
         lines-all (apply + (for [[_ v] (sort-by key actions-all2)]
                              (count v)))
         pos-worksheet-build-map (fn [b]
                                   (let [x 20 ;(rand-int 120) ;20
                                         worksheet-name (str (nth worksheet-keys b))
                                         lines-up-to-now (apply + (for [[k v] (sort-by key actions-all2)
                                                                        :when (< k b)]
                                                                    (count v)))
                                         fieldsets (for [[k v] (wk-actions-wmap worksheet-name)] (vec (flatten [k v])))
                                         y (if (= lines-up-to-now 0)
                                             0
                                             (+ (* b 60) (* 27 lines-up-to-now)))
                                         fieldsets-map (into {} (for [f (range (count fieldsets))]
                                                                  {(first (nth fieldsets f))
                                                                   {:x (+ 360 x) ;; 300 is block width
                                                                    :y (+ 62 (if (= f 0) y  ;; 66 is header height
                                                                                 (+ y (* f 21))))}}))
                                         flags (remove nil? (-> []
                                                                (conj (if (some #(= % worksheet-name)
                                                                                source-worksheets) :source-sheet? nil))))]
                                     {worksheet-name
                                      {:type :source-worksheet
                                       :id b :x x :y y
                                       :fieldsets fieldsets
                                       :fieldsets-map fieldsets-map
                                       :name worksheet-name
                                       :flags flags}}))
         pos-worksheet-blocks {:source-worksheet (into {} (for [b (range cnt-wks)]
                                                            (pos-worksheet-build-map b)))}

        ;; target-worksheets
         dash-sheets @(re-frame/subscribe [::objects-in-dashboard selected-dash :worksheets])
         worksheets (select-keys worksheets dash-sheets)
         worksheet-keys (remove nil? (keys worksheets))
         worksheet-filters (filter #(= selected-dash (get (last %) :dashboard)) worksheet-filters)
         worksheet-filter-keys (set (for [f (remove nil? (keys worksheet-filters))] (first f)))
         cnt-wks (count worksheet-keys)
         params-up          (into {} (for [[k v] (into {} (for [[k v] (into {} worksheet-filters)] {(first k) v}))]
                                       (when (find v :param-domain-type)
                                         {k (vec (remove nil? (for [[k1 v1] (get-in db [:twb-root :worksheets])
                                                                    :when (some #(= k %) (deep-flatten v1))]
                                                                k1)))})))
         params-up-assoc (distinct (flatten (for [[_ v] params-up] v)))
         params-up-assoc1 (into {} (for [e params-up-assoc] {e (vec (remove nil? (for [[k v] params-up] (when (some #(= e %) v) k))))}))
         field-log (into {} (for [b (range (count worksheet-keys))]
                              {b (let [worksheet-name (str (nth worksheet-keys b))
                                       params-upstream (get params-up-assoc1 worksheet-name)
                                       filters-poss (cset/intersection (set (get worksheet-filter-targets worksheet-name)) worksheet-filter-keys)
                                       fieldsets (vec (remove nil? (merge (for [[k v] (wk-actions-wmap worksheet-name)] (vec (flatten [k v])))
                                                                          (vec (cons :filters filters-poss))
                                                                          (when (not (nil? params-upstream))
                                                                            (vec (flatten [:upstream-parameter params-upstream]))))))]
                                   fieldsets)}))
         pos-target-worksheets {:target-worksheet (into {} (for [b (range (count worksheet-keys))]
                                                             (let [x 980
                                                                   worksheet-name (str (nth worksheet-keys b))
                                                                   lines-up-to-now (apply + (for [[k v] (sort-by key field-log)
                                                                                                  :when (< k b)]
                                                                                              (count v)))
                                                                   fieldsets (get field-log b)
                                                                   fieldsets (remove #(= % [:filters]) fieldsets)
                                                                   y (if (= lines-up-to-now 0)
                                                                       0
                                                                       (+ (* b 60) (* 27 lines-up-to-now)))
                                                                   fieldsets-map (into {} (for [f (range (count fieldsets))]
                                                                                            {(first (nth fieldsets f))
                                                                                             {:x (+ 0 x) ;; 300 is block width
                                                                                              :y (+ 66 (if (= f 0) y  ;; 66 is header height
                                                                                                           (+ y (* f 20))))}}))
                                                                   flags (remove nil? (-> []
                                                                                          (conj (if (some #(= % worksheet-name) target-worksheets-all) :target-sheet? nil))
                                                                                          (conj (if (some #(= % worksheet-name) target-worksheets-selected) :target-sheet-selected? nil))))]
                                                               {worksheet-name {:type :target-worksheet
                                                                                :id b :x x :y y
                                                                                :fieldsets fieldsets
                                                                                :fieldsets-map fieldsets-map
                                                                                :name worksheet-name
                                                                                :flags flags}})))}

        ;;; worksheet filters
         worksheet-filters (filter #(= selected-dash (get (last %) :dashboard)) worksheet-filters)
         worksheet-filter-keys (remove nil? (keys worksheet-filters))
         pos-worksheet-filters {:worksheet-filter (into {} (for [b (range (count worksheet-filter-keys))]
                                                             (let [x 20
                                                                   fieldsets [[:filter nil]]
                                                                   filter-data (get (into {} worksheet-filters) (nth worksheet-filter-keys b))
                                                                   param? (find filter-data :param-domain-type)
                                                                   worksheet-filter-name (str (first (nth worksheet-filter-keys b)))
                                                                   target-sheets (if param?
                                                                                   (vec (remove nil? (for [[k1 v1] (get-in db [:twb-root :worksheets])]
                                                                                                       (when (some #(= worksheet-filter-name %) (deep-flatten v1))
                                                                                                         k1))))
                                                                                   (apply concat
                                                                                          (for [[k v] (get pos-target-worksheets :target-worksheet)]
                                                                                            (remove nil?   (for [f (get v :fieldsets)]
                                                                                                             (when (and (= (first f) :filters)
                                                                                                                        (some #(= % worksheet-filter-name) (rest f)))
                                                                                                               k))))))
                                                                   y (+ (+ (* cnt-wks 60) (* lines-all 27))
                                                                        (* b 75))
                                                                   fieldsets-map (into {} (for [f (range (count fieldsets))]
                                                                                            {(first (nth fieldsets f))
                                                                                             {:x (+ 360 x) ;; 300 is block width
                                                                                              :y (+ 40 (if (= f 0) y  ;; 40 is header height
                                                                                                           (+ y (* f 20))))}}))]
                                                               ;(tap> [:ff worksheet-filter-name target-sheets])
                                                               {worksheet-filter-name (merge {:type (if param? :parameter :worksheet-filter)
                                                                                              :id b :x x :y y
                                                                                              :caption (get filter-data :caption (str "(" (get filter-data :role) ")"))
                                                                                              :fieldsets fieldsets
                                                                                              :color (let [scm (if param?
                                                                                                                 @param-scheme ;:Greens 
                                                                                                                 @filter-scheme)
                                                                                                           the-keys (count worksheet-filter-keys)
                                                                                                           ks (vec (filter integer? (for [n (keys (get db/colorbrewer scm))]
                                                                                                                                      (js/parseInt (unkeyword n)))))
                                                                                                           mnks (apply min ks)
                                                                                                           mxks (apply max ks)
                                                                                                           ckey (cond (< the-keys mnks) mnks
                                                                                                                      (> the-keys mxks) mxks
                                                                                                                      :else the-keys)
                                                                                                           cnts (keyword (str ckey))]
                                                                                                ;(tap> [the-keys ks mnks mxks ckey scm cnts])
                                                                                                       (get-in db/colorbrewer [scm cnts b]))
                                                                                              :target-sheets target-sheets
                                                                                              :fieldsets-map fieldsets-map
                                                                                              :name worksheet-filter-name
                                                                                              :flags []}
                                                                                             (if (find filter-data :from-action)
                                                                                               ;; params spawned from action edit calls
                                                                                               {:from-action (get filter-data :from-action)
                                                                                                :ttype (get filter-data :param-domain-type)} {}))})))}

         params          {:parameters (into {} (for [[k v] (get pos-worksheet-filters :worksheet-filter)
                                                     :when (= (get v :type) :parameter)]
                                                 {k (merge v
                                                           {:downstream (vec (remove nil? (for [[k1 v1] (get-in db [:twb-root :worksheets])
                                                                                                :when (some #(= k %) (deep-flatten v1))] k1)))}
                                                           {:deep-downstream (vec (remove nil? (for [[k1 v1] (get-in db [:twb-root :worksheets])
                                                                                                     :when (cstr/includes? (str v1) (str k))] k1)))})}))}]
     ;(tap> [:params-up  params-up params-up-assoc params-up-assoc1])
     (merge pos-action-blocks pos-worksheet-blocks pos-worksheet-filters pos-target-worksheets params))))


(re-frame/reg-sub
 ::port-coords
 (fn [_ [_ selected-dash]]
   (let [position-map @(re-frame/subscribe [::position-map selected-dash]) ;; TODO remove subs in subs, but in this small app it's not a big deal
         source->action-coords (vec (apply concat (for [[k v] (get position-map :source-worksheet)]
                                                    (for [[k1 v1] (get v :fieldsets-map)]
                                                      [(get v1 :x) (get v1 :y)
                                                       (get-in position-map [:action k1 :fieldsets-map :filter-left :x])
                                                       (get-in position-map [:action k1 :fieldsets-map :filter-left :y])
                                                       (get-in position-map [:action k1 :color]) [:source-worksheet k] [:action k1]]))))
         action->target-coords (vec (apply concat (for [[k v] (get position-map :target-worksheet)]
                                                    (for [[k1 v1] (get v :fieldsets-map)
                                                          :when (not (= (get-in position-map [:action k1 :command]) :edit-parameter-action))]
                                                      [(get-in position-map [:action k1 :fieldsets-map :filter-right :x])
                                                       (get-in position-map [:action k1 :fieldsets-map :filter-right :y])
                                                       (get v1 :x) (get v1 :y)
                                                       (get-in position-map [:action k1 :color]) [:action k1] [:target-worksheet k]]))))
         action->special-coords (vec (apply concat (for [[k v] (get position-map :parameters)
                                                         :when (= (get v :ttype) :edit-parameter-action)]
                                                     (for [[k1 _] (get-in position-map [:action])
                                                           :when (= k1 (get v :from-action))]
                                                       [(get-in position-map [:action (get v :from-action) :fieldsets-map :filter-right :x])
                                                        (get-in position-map [:action (get v :from-action) :fieldsets-map :filter-right :y])
                                                        (get v :x) (+ (get v :y) 30)
                                                        (get-in position-map [:action (get v :from-action) :color])
                                                        [:action (get v :from-action)]
                                                        [:parameter k]]))))        
         filter->target-coords (vec (apply concat (for [[k v] (get position-map :worksheet-filter)]
                                                    (for [[_ v1] (get v :fieldsets-map)]
                                                      (for [tg (get v :target-sheets)
                                                            :let [param? (= (get v :type) :parameter)
                                                                  tg-x (get-in position-map [:target-worksheet tg :fieldsets-map
                                                                                             (if param? :upstream-parameter :filters)
                                                                                             :x])
                                                                  tg-y (get-in position-map [:target-worksheet tg :fieldsets-map
                                                                                             (if param? :upstream-parameter :filters)
                                                                                             :y])
                                                                  tg-color (get v :color)]]
                                                          [(get v1 :x) (get v1 :y)
                                                           tg-x
                                                           tg-y
                                                           tg-color [(if param? :parameter :worksheet-filter) k] 
                                                           [:target-worksheet tg]])))))
         filter->target-coords (apply concat filter->target-coords)
         coords (vec (filter #(not (nil? (nth % 0))) ;; coords w no source...
                             (into action->special-coords (into filter->target-coords (into source->action-coords action->target-coords)))))]
     ;(tap> [:action->special-coords action->special-coords])
     (tap> coords)
     coords)))


(re-frame/reg-sub
 ::relations
 (fn [_ [_ selected-dash]]
   (let [coords @(re-frame/subscribe [::port-coords selected-dash])
         pairs (into [] (for [c coords] [(first (take-last 2 c)) (last (take-last 2 c))]))
         kpairs (into {} (for [s (distinct (for [[v1 _] pairs] v1))]
                           {s (vec (distinct (apply concat (filter #(= (first %) s) pairs))))}))
         seconds (vec (for [[k v] pairs]
                        (into [k v] (get kpairs v))))]
     seconds)))

(re-frame/reg-sub
 ::action-activation
 (fn [db [_ action-name]]
   (let [type (get-in db [:twb-root :actions action-name :activation-attrs :type])
         auto-clear (get-in db [:twb-root :actions action-name :activation-attrs :auto-clear])]
     (str type (if (not (nil? auto-clear)) (str " (auto-clear " auto-clear ")") "")))))

(defn box [type id name x y flags fieldsets color]
  (let [;rows (count fieldsets)
        selected-dash @(re-frame/subscribe [::selected-dash])
        position-map @(re-frame/subscribe [::position-map selected-dash])
        ttype (try (first type) (catch :default _ type))
        tid [ttype name]
        selected  @(re-frame/subscribe [::selected])
        selected? (= tid selected)
        nothing-selected? (nil? selected)
        relations @(re-frame/subscribe [::relations selected-dash])
        involved (vec (remove nil? (distinct (apply concat (filter #(some (fn[x] (= x tid)) %) relations)))))
        color (if (nil? color)
                @basic-block-color  ;"#D8BFD8" ;"#E6E6FA" ;"#eeeeee"
                color)
        involved? (some #(= selected %) involved)]
    ^{:key (str tid name)}
    [re-com/v-box
     :attr {:on-click #(re-frame/dispatch [::select (if selected? nil tid)])}
     :size "auto"
     :width "360px" ;(px (+ 300 (* rows 22)))
     :style {:color "#000000"
             :border-radius "10px"
             :box-shadow (if selected? (str "0 0 22px 6px " color) "")
             :background-color (if (or selected? nothing-selected? involved?)
                                 color "#E6E6FA55")
             :left x :top y
             :position "fixed"}
     :children [[re-com/box
                 :style {:font-size "9px"
                         :font-weight 700}
                 :child (if (and (try (= (count type) 2) (catch :default _ false))
                                 (vector? type))
                          [re-com/h-box
                           :justify :between
                           :children [[re-com/box :child (str (first type))
                                       :style {:font-weight 700}]
                                      [re-com/box :child (str (last type))]]]
                          [re-com/box :child (str type)
                           :style {:font-weight 700}])
                 :size "none" :padding "3px"
                 :style {:background-color "#00000015"
                         :font-size "10px"}
                 :height "23px"]

                [re-com/h-box
                 :justify :between
                 :children [[re-com/md-icon-button :src (at)
                             :md-icon-name (cond (= ttype :worksheet-filter) "zmdi-filter-list"
                                                 (= ttype :parameter) "zmdi-puzzle-piece"
                                                 (= ttype :action) "zmdi-movie-alt"
                                                 :else "zmdi-view-dashboard")
                             :style {:opacity 0.5 :padding-left "2px"}]

                            [re-com/box
                             :style {:font-weight 700}
                             :padding "3px"
                             :size "none"
                             :child (str name)]]]

                (when (and (or (= ttype :source-worksheet)
                               (= ttype :target-worksheet))
                           (not (empty? fieldsets)))

                  [re-com/v-box
                   :style {:padding-bottom "3px"}
                   :gap "2px"
                   :children (for [fieldset fieldsets]
                               (let [action-name (first fieldset)
                                     thid (cond (= action-name :filters) [:worksheet-filter (nth fieldset 1)]
                                                (= action-name :upstream-parameter) [:parameter (nth fieldset 1)]
                                                :else [:action action-name])
                                     line-involved (vec (remove nil? (distinct (apply concat (filter #(some (fn [x] (= x thid)) %) relations)))))
                                     line-involved? (some #(= selected %) line-involved)
                                     action-color (get-in position-map [:action action-name :color])
                                     label-box [re-com/box
                                                :child (str action-name " (" (count fieldset) " " (count fieldsets) ")")
                                                :style {:padding-top "2px"
                                                        :opacity 0.6
                                                        :font-weight 700
                                                        :font-size "8px"}]
                                     field-box [re-com/box
                                                :style {:background-color "#00000017"
                                                        :border-radius "5px"
                                                        :font-weight 500
                                                        :font-size "9px"}
                                                :child [re-com/h-box
                                                        :size "auto"
                                                        :gap "5px"
                                                        :children (for [f (rest fieldset)]
                                                                    [re-com/box
                                                                     :padding "3px"
                                                                     :style {:background-color (if (or (= action-name :filters)
                                                                                                       (= action-name :upstream-parameter))
                                                                                                 (get-in position-map [:worksheet-filter (str f) :color])
                                                                                                 action-color)
                                                                             :opacity (if (or selected? nothing-selected?
                                                                                              (and involved? ; (or  ;; TODO, some fkdup logic here
                                                                                                             ; (= (str f) (last thid))
                                                                                                             ; ;(some #(= (str f) %) fieldset)
                                                                                                             ;    line-involved?)
                                                                                                   (or (= (last selected) (str f))
                                                                                                       (if (= (first selected) :worksheet-filter)
                                                                                                         nil ;; TODO - eyes emoji  
                                                                                                         line-involved?)))) 1.0 0.3)
                                                                             :border-radius "5px"
                                                                             :color "#000000"
                                                                             :font-weight 700
                                                                             :font-size "7px"}
                                                                     :child (str f)])]]]
                                 [re-com/h-box
                                  :gap "5px"
                                  :justify :between
                                  :style {:padding-left "3px" :padding-right "3px" :padding-top "3px"
                                          ;:background-color "maroon"
                                          :border-top "1px dashed #00000045"}
                                  :children (if (= ttype :source-worksheet)
                                              [label-box field-box]
                                              [field-box label-box])]))])

                (when (and (= ttype :worksheet-filter)
                           (not (empty? fieldsets)))
                  [re-com/box
                   :height "15px"
                   :align :center :justify :center
                   :style {:opacity 0.4
                           :font-weight 700
                           :font-size "8px"}
                   :child "filter input"])

                (when (and (= ttype :parameter)
                           (not (empty? fieldsets)))
                  [re-com/box
                   :height "15px"
                   :align :center :justify :center
                   :style {:opacity 0.4
                           :font-weight 700
                           :font-size "8px"}
                   :child "parameter input"])

                (when (= ttype :action)

                  [re-com/box
                   :height "15px"
                   :style {:padding-left "6px"
                           :opacity 0.4
                           :font-weight 700
                           :font-size "10px"}
                   :child @(re-frame/subscribe [::action-activation (str name)])])

                [re-com/gap :size "4px"]]]))

(defn draw-lines [coords]
  (doall (for [[x1 y1 x2 y2 color n1 n2] coords]
           ^{:key (hash (str x1 y1 x2 y2 "lines"))}
           (let [selected-dash @(re-frame/subscribe [::selected-dash])
                 relations @(re-frame/subscribe [::relations selected-dash])
                 selected  @(re-frame/subscribe [::selected])
                 involved (vec (remove nil? (distinct (apply concat (filter #(some (fn [x] (= x selected)) %) relations)))))
                 nothing-selected? (nil? selected)
                 involved1? (some #(= n1 %) involved)
                 involved2? (some #(= n2 %) involved)
                 involved? (and involved1? involved2?)]
             [:path {:stroke-width 13
                     :stroke (if (or involved? nothing-selected?)
                               (if (nil? color) "pink" color) "#E6E6FA22")
                     :fill "none"
                     :filter "drop-shadow(0.25rem 0.35rem 0.4rem rgba(0, 0, 0, 0.44))"
                     :d (curved-path-h x1 y1 x2 y2)}]))))

(defn link-buttons [bg-color]
  [re-com/h-box
   :padding "4px"
   :size "none"
   :height "60px"
   :style {:background-color bg-color
           :margin-right "-8px"
           :z-index 99980}
   :children [[re-com/md-icon-button
               :md-icon-name "zmdi-twitter"
               :on-click #(js/window.open "https://twitter.com/ryrobes" "_blank" nil)
               :style {:opacity 0.5
                       :color "#d8bfd8"
                       :padding-left "14px"
                       :padding-right "8px"
                       :font-size "33px"}]
              [re-com/md-icon-button
               :md-icon-name "zmdi-github"
               :on-click #(js/window.open "https://github.com/ryrobes/tabflowmaps" "_blank" nil)
               :style {:opacity 0.5
                       :color "#d8bfd8"
                       :padding-right "8px"
                       :font-size "33px"}]]])

(defn dashboard-menu [dashboards selected-dash]
  [re-com/box
   :padding "9px"
   :style {:z-index 9999}
   :size "none"
   :child [re-com/h-box
           :size "auto"
           :justify :between
           :children [[re-com/h-box
                       :size "auto"
                       :gap "10px"
                       :children (for [d dashboards
                                       :let [selected? (= (str d) (str selected-dash))
                                             pos-map @(re-frame/subscribe [::position-map d])
                                             worksheets-cnt (count (keys (get pos-map :source-worksheet)))
                                             actions-cnt (count (keys (get pos-map :action)))
                                             filters-cnt (count (keys (get pos-map :worksheet-filter)))
                                             params-cnt (count (keys (get pos-map :parameters)))]]
                                   [re-com/box
                                    :align :center :justify :center
                                    :attr {:on-click #(do (re-frame/dispatch [::select-dash (str d)])
                                                          (re-frame/dispatch [::select nil]))}
                                    :child [re-com/h-box
                                            :padding "2px"
                                            :height "40px"
                                            :gap "10px"
                                            :children [[re-com/box :child (str d) :padding "1px" :style {:font-size "19px"}]
                                                       [re-com/v-box
                                                        :children [[re-com/box :child (str " sheets: " worksheets-cnt) :style {:opacity 0.6 :font-size "11px"}]
                                                                   [re-com/box :child (str " actions: " actions-cnt) :style {:opacity 0.6 :font-size "11px"}]]]
                                                       [re-com/v-box
                                                        :children [[re-com/box :child (str " filters: " filters-cnt) :style {:opacity 0.6 :font-size "11px"}]
                                                                   [re-com/box :child (str " params: " params-cnt) :style {:opacity 0.6 :font-size "11px"}]]]]]
                                    :style {:font-weight 700
                                            :padding-left "12px" :padding-right "12px"
                                            :margin-bottom "-20px"
                                            :border-radius "12px 12px 0px 0px"
                                            :cursor "pointer"
                                            :color (if selected? "#ffffff" "#000000")
                                            :background-color (if selected? "#000000" "#a3a3a365")}])]
                      [link-buttons "#302c41"]]]
   :width "100%" :height "60px"
   :style {:positon "fixed" :left 0 :top 0
           :overflow "hidden"
           :background-color "#302c41"}])

(defn color-dd [atm label]
  (let [cc (vec (for [e (keys db/colorbrewer)]
                  {:id e :label e :group (get-in db/colorbrewer [e :type])}))]
    [re-com/v-box
     :children
     [[re-com/box :align :center :justify :center
       :padding "2px"
       :child (str label) :style {:opacity 0.5}]
      [re-com/single-dropdown
       :width "200px"
       :can-drop-above? true
       :style {:height "40px"}
       :model atm
       :on-change #(reset! atm %)
       :choices cc]]]))

(defn options-box []
       [re-com/box
        :size "auto"
        :padding "8px"
        :justify :center
        :child [re-com/v-box
                :gap "4px"
                :children [[re-com/box
                            :height "10px"
                            :width "200px"
                            :style {:cursor "pointer"}
                            :attr {:on-click #(reset! options? (not @options?))}
                            :justify :center :align :center
                            :child [re-com/md-icon-button
                                    :md-icon-name (cond @options? "zmdi-caret-down"
                                                        :else "zmdi-caret-up")
                                    :style {:opacity 0.5 :padding-left "2px"}]]

                           (when @options? [color-dd action-scheme "action color scheme" 900])
                           (when @options? [color-dd filter-scheme "filter color scheme" 800])
                           (when @options? [color-dd param-scheme "parameter color scheme" 700])
                           (when @options?

                             [re-com/v-box
                              :gap "3px"
                              :align :center :justify :center
                              :children
                              [[re-com/h-box
                                :padding "2px"
                                :gap "6px"
                                :align :center :justify :center
                                :children [[re-com/box :child (str "base block color")]
                                           [re-com/box :child (str @basic-block-color)
                                            :align :center :justify :center
                                            :style {:background-color @basic-block-color
                                                    :color "#000000"
                                                    :font-weight 700
                                                    :font-size "12px"
                                                    :border-radius "4px"}
                                            :padding "3px"
                                            :width "60px"
                                            :height "18px"]] :style {:opacity 0.5}]
                               [(reagent/adapt-react-class react-colorful/HexColorPicker)
                                {:color @basic-block-color
                                 :onChange #(reset! basic-block-color %)}]]])
                           (when @options? [re-com/gap :size "10px"])]]
        :style {:color "#ffffff"
                :background-color "#302c4180" ; "#eeeeee15"
                :z-index 39900
                :border-radius "0px 0px 0px 12px"
                :position "fixed" :right 0 :top 60} :width "250px"])

(defn workbook-title []
  [re-com/box
   :child (str @(re-frame/subscribe [::file-name]))
   :padding "8px 15px 8px 18px"
   :size "auto"
   :style {:color "#ffffff"
           :background-color "#302c4180" ; "#eeeeee15"
           :z-index 39900
           :font-size "17px"
           :border-radius "12px 0px 0px 0px"
           :position "fixed" :right 0 :bottom 0}
   :height "40px"])

(defn spinny []
  (let [w @(re-frame/subscribe [::bp/screen-width])
        h @(re-frame/subscribe [::bp/screen-height])
        hhh 120
        www 120
        hh (- (js/Math.floor (/ h 2)) (/ hhh 2))
        ww (- (js/Math.floor (/ w 2)) (/ www 2))]
    [re-com/box
     :style {:left ww :top hh :position "fixed"
             :z-index 99999
             :font-size "19px"
             :color "white"}
     :align :center :justify :center
     :height (px hhh)
     :width (px www)
     :child [re-com/throbber :size :large
             :color "#ffffff"]]))

(defn hello-modal []
  (let [w @(re-frame/subscribe [::bp/screen-width])
        h @(re-frame/subscribe [::bp/screen-height])
        hhh 370
        www 720
        hh (- (js/Math.floor (/ h 2)) (/ hhh 2))
        ww (- (js/Math.floor (/ w 2)) (/ www 2))]
    [re-com/box
     :style {:left ww :top hh :position "fixed"
             :z-index 99999
             :font-size "19px"
             :border (str "12px solid " "#302c41")
             :background-color "#302c41" ;"#000000"
             :border-radius "15px"
             ;:filter "drop-shadow(0.6rem 0.6rem 0.6rem rgba(0, 0, 0, 0.66))"
             :box-shadow (str "0 0 50px 15px " "#000000")
             :color "white"}
     ;:align :center :justify :center
     ;:height (px hhh)
     :width (px www)
     :child [re-com/v-box 
             :size "auto"
             :style {:background-color "#000000"
                     :border-radius "12px"}
             :children [[re-com/h-box 
                         :height "30px"
                         ;:width "490px"
                         :padding "4px"
                         :size "none"
                         :children [[re-com/box :child "tabflowmaps 0.1.0 - tableau action relationship viewer" 
                                     :size "auto" 
                                     :style {:padding-left "6px"
                                             :color "#302c41"
                                             :padding-top "1px"
                                             :font-size "15px"}]
                                    [re-com/box
                                     :align :end
                                     :child [re-com/md-icon-button :src (at)
                                             :on-click #(reset! hello? false)
                                             :md-icon-name "zmdi-close"
                                             :style {:color @basic-block-color
                                                     :padding-top "2px"}]]]]
                        [re-com/v-box 
                         :padding "10px"
                         :gap "10px"
                         :children [[re-com/box 
                                     :align :center :justify :center
                                     :style {:padding-top "13px" :padding-bottom "13px"}
                                     :child [:img {:src "images/tfm-logo.png" :width 562 :height 64}]]
                                    [re-com/box 
                                     :align :center :justify :center
                                     :padding "10px"
                                     :style {:font-size "25px" 
                                             :font-weight 700
                                             :color @basic-block-color}
                                     :child "drag any TWB file on to the canvas"]
                                    [re-com/box :style {:font-size "15px"
                                                        :font-weight 600
                                                        :padding-top "14px"
                                                        :color "#302c41"}
                                     :child "*Not tested widely, there are lots of bugs (please file issues on the GH page)."]
                                    [re-com/box :style {:font-size "15px"
                                                        :font-weight 600
                                                        :color "#302c41"}
                                     :align :start :justify :start
                                     :child "*Web-server does *not* save your file."]
                                    [re-com/box 
                                     :align :center :justify :center
                                     :child [link-buttons "#00000000"]]
                                    ]]]]]))

(defn dimmed-bg []
  (let [w @(re-frame/subscribe [::bp/screen-width])
        h @(re-frame/subscribe [::bp/screen-height])]
    [re-com/box :size "none" :child " "
     :style {:background "#00000099" 
             :width (px w)
             ;:filter "blur(5px)"
             :height (px h)
             :position "fixed" :left 0 :top 0 :z-index 99990}]))

(dnd2/subscribe! (js/document.querySelector "div") :canvas02
                 {:drop  (fn [_ files]
                           (when (not (= "" (-> files .-value)))
                             (let [^js/File file0 (-> files (aget 0))
                                   fname (-> file0 .-name)]
                               (tap> [:run? (cstr/lower-case fname)])
                               (when (cstr/ends-with? (cstr/lower-case fname) ".twb")
                                 #_{:clj-kondo/ignore [:redundant-do]}
                                 (do ;; proc file, reset var first to prevent double fns
                                   (set! (-> files .-value) "")
                                   (tap> [:saving-csv-to-server fname])
                                   (http/read-file file0 #(re-frame/dispatch [::http/proc-twb fname (str %)]))
                                   (set! (-> files .-value) ""))))))})



(defn main-panel []
  (let [running? @(re-frame/subscribe [::req-running?])
        dashboards (keys @(re-frame/subscribe [::dashboards]))
        selected-dash @(re-frame/subscribe [::selected-dash])
        position-map @(re-frame/subscribe [::position-map selected-dash])
        coords @(re-frame/subscribe [::port-coords selected-dash])]
    (tap> position-map)
    [(reagent/adapt-react-class zpan/TransformWrapper)
     {:limitToBounds false
      ;:centerZoomedOut true
      :disabled false
      :onPanningStop #(reset! db/pan-zoom-offsets [(.-positionX (.-state %)) (.-positionY (.-state %)) (.-scale (.-state %))])
      :onWheelStop #(reset! db/pan-zoom-offsets [(.-positionX (.-state %)) (.-positionY (.-state %)) (.-scale (.-state %))])
      :onDoubleClick #(reset! db/pan-zoom-offsets [0 0 1])
      :doubleClick {:mode "reset"}
      :initialScale (nth @db/pan-zoom-offsets 2)
      :initialPositionX (nth @db/pan-zoom-offsets 0)
      :initialPositionY (nth @db/pan-zoom-offsets 1)
      :wheel {:step 0.1}
      :minScale 0.025}

     [dashboard-menu dashboards selected-dash]
     [options-box]
     (when running? [spinny])
     (when @hello? [hello-modal])
     (when @hello? [dimmed-bg])
     [workbook-title]

     [(reagent/adapt-react-class zpan/TransformComponent)
      [:div#based
       [:svg
        {:style {:width "5200px" ;; big ass canvas so nothing gets cut off svg-wise
                 :height "20200px"
                 :z-index 9999}}
        (draw-lines coords)]
 
       (let [worksheet-blocks (for [[_ {:keys [fieldsets flags id name type x y]}]
                                    (get position-map :source-worksheet)]
                                [box type id name x y flags fieldsets])
             worksheet-filter-blocks (for [[_ {:keys [fieldsets id name caption color type x y]}]
                                           (get position-map :worksheet-filter)]
                                       [box [type caption] id name x y nil fieldsets color])
             action-blocks (for [[_ {:keys [command flags id name type x y color]}]
                                 (get position-map :action)]
                             [box [type command] id name x y flags nil color])
             target-worksheet-blocks (for [[_ {:keys [fieldsets flags id name type x y]}]
                                           (get position-map :target-worksheet)]
                                       [box type id name x y flags fieldsets])]
         [re-com/v-box ;; blocks are position fixed, so v/h-box doesnt matter
          :children [[re-com/v-box :children (into worksheet-blocks worksheet-filter-blocks)] 
                     [re-com/v-box :children action-blocks]
                     [re-com/v-box :children target-worksheet-blocks]]])]]]))
