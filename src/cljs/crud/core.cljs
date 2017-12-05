(ns crud.core
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [ajax.core :refer [POST GET DELETE PUT]]
            [ajax.edn :as edn]
            [accountant.core :as accountant]))

;; -------------------------
;; Views


(defn params-to-paginated-keyword [params]
  (-> params
      str
      keyword))

(defn get-all [{:keys [params atom]}]
  (-> @atom (get-in  [(params-to-paginated-keyword params) :items])))

(defn get-page [_atom]
  (get-in @_atom [:params :page]))

(defn remove-from-list [eid _atom]
  (let [ptpkw (params-to-paginated-keyword (:params @_atom))
        items (get-in  @_atom [ptpkw :items])
        without (->> items
                     (filter #(not= eid %)))]
    (swap! _atom #(assoc-in % [ptpkw :items] without))))

(defn set-page [_atom page]
  (swap! _atom (fn [e] (assoc-in e [:params :page] page))))

(defn items-at-key [{:keys [atom params]}]
  (get-in @atom [(params-to-paginated-keyword params)]))

(defn fetch-all [{:keys [method url atom params force] :as arg} ]
  (swap! atom #(dissoc % :error))
  (let [method (or method GET)
        do-fetch? (if force
                    true
                    (if (not (items-at-key arg ))
                      true
                      false))]
    (when do-fetch?
      (method url
              {:format (edn/edn-request-format {:keywords? true})
               :response-format (edn/edn-response-format )
               :params params
               :handler (fn [{:keys [total items aggs error]}]
                          (if error
                            (swap! atom #(assoc % :error error))
                            (let [_key (params-to-paginated-keyword params)
                                  dict (reduce (fn [memo item]
                                                 (assoc memo (:db/id item) item)
                                                 ) {} items )
                                  ids (map :db/id items)]
                              (swap! atom
                                     (fn [e]
                                       (-> e
                                           (assoc :dict (merge (:dict e) dict))
                                           (assoc-in [_key] {:total total
                                                             :aggs aggs
                                                             :items (concat (:items e) ids)})))))))}))))

(defn do-fetch [url _atom & [{:keys [method]}]]
  (let [method (or method POST)]
    (fetch-all {:url url
                :force true
                :method method
                :atom _atom
                :params (:params @_atom)} )))

(defn _fetch [id url _atom & [opts]]
  (let [opts (or opts {})
        params (merge opts  {:url url
                             :method POST
                             :atom _atom
                             :params (:params @_atom)})]
    (fetch-all  params)))

(defn fetch [id url _atom]
  (fetch-all {:url url
              :force true
              :method POST
              :atom _atom
              :params (:params @_atom)} ))

(defn get-aggs [{:keys [atom]}]
  (let [params (-> @atom :params params-to-paginated-keyword)]
    (get-in @atom [params :aggs])))

(defn n-get-aggs [{:keys [atom ]}]
  (let [params (-> @atom :params params-to-paginated-keyword)]
    (get-in @atom [params :aggs])))

(defn _get-aggs [_atom]
  (get-aggs {:params (:params @_atom)
             :atom _atom}))

(defn aggs-config [_atom map-func]
  (let [aggs (_get-aggs _atom)]
    (->> aggs
         keys
         (map (fn [k]
                {:key k
                 :items (let [agg (->> k
                                       (get aggs)
                                       (sort-by :doc_count)
                                       reverse
                                       (map #(map-func % k))
                                       vec
                                       )
                              ]
                          agg
                          )}))
         vec)))

(defn fetch-item [{:keys [url atom method params]}]
  (let [method (or method GET)
        params (or params {})]
    (method url
            {:format (edn/edn-request-format {:keywords? true})
             :params params
             :handler (fn [res]
                        (let [dict (assoc {} (:db/id res) res)]
                          (swap! atom #(-> %
                                           (assoc :dict (merge (:dict %) dict))))))})))

(defn fetch-details [url id _atom & [{:keys [force callback]}]]
  (when (or
         force
         (not (get-in @_atom [:dict id])))
    (POST url {:params {:id id}
               :format (edn/edn-request-format {:keywords? true})
               :response-format (edn/edn-response-format)
               :handler (fn [res]
                          (let [dict (assoc {} (:db/id res) res)]

                            (swap! _atom #(-> %
                                              (assoc :dict (merge (:dict %) dict))

                                              ))
                            (when callback (callback))))})))

(defn get-items [{:keys [atom accessor]}]
  (get-in @atom [accessor :items] ))

(defn get-item [{:keys [atom accessor]}]
  (get-in @atom (concat [:dict ] accessor )))

(defn update-state [{:keys [atom accessor params] :as config}]
  (let [item (get-item config)
        merged (merge item params)]
    (swap! atom #(assoc-in % (concat [:dict ] accessor ) merged))))

(defn update-item [{:keys [url atom accessor id ent params latency method]}]
  (let [method (or method POST)
        func (fn [] (method url {:format (edn/edn-request-format {:keywords? true})
                              :response-format (edn/edn-response-format )
                              :handler (fn [res]
                                         (swap! atom (fn [_at]
                                                       (let [item (merge
                                                                   (get-in _at [:dict id])
                                                                   (assoc res :loading false))]
                                                         (assoc-in _at [:dict id ] item)))))
                              :params params}))]
    (swap! atom (fn [e] (assoc-in e [:dict id :loading] true)))
    (if latency
      (.setTimeout js/window (fn [] (func)) latency)
      (func))))

(defn _get [_atom]
  (get-all {:params (:params @_atom)
            :atom _atom} ))

(defn _get-item [_atom id]
  (get-item {:atom _atom
             :accessor [id]}))

(defn get-total [_atom]
  (get-in @_atom [(params-to-paginated-keyword (:params @_atom)) :total]))


(defn create [{:keys [_atom method params url into-atom]}]
  (let [method (or method POST)]
    (swap! _atom #(dissoc % :error))
    (method url {:format (edn/edn-request-format {:keywords? true})
                 :response-format (edn/edn-response-format )
                 :handler (fn [res]
                            (if-let [error (:error res)]
                              (swap! _atom #(assoc % :error error))
                              (let [dict (:dict @into-atom)
                                    dict (assoc dict (:db/id res) res)]
                                (swap! into-atom #(assoc % :dict dict)))                              ))
                 :params params})))

(defn delete [{:keys [method params url into-atom]}]
  (let [method (or method POST)]
    (method url {:format (edn/edn-request-format {:keywords? true})
                 :response-format (edn/edn-response-format )
                 :handler (fn [res]
                            (when-not (:error res)
                              (let [dict (:dict @into-atom)
                                    dict (dissoc dict (:db/id res))
                                    items (:items @into-atom)
                                    items (->> items
                                               (filter #(not= % (:db/id params)))
                                               vec)]
                                (swap! into-atom #(assoc %
                                                         :items items
                                                         :dict dict)))))
                 :params params})))



(defn pagination [page total callback pp]
  (let [ap (range
            (/ total pp))
        tp (count ap)
        fp (first ap)
        lp (last ap)
        new-list (reduce (fn [memo item]
                           (conj memo [item item])
                           ) [] (->> ap
                                     (drop (- page 4))
                                     (take 9)))
        new-list (if (= 0 page)
                   new-list
                   (concat [["<<" 0]] new-list ))
        new-list (if (= page lp)
                   new-list
                   (concat new-list [[">>" lp]] ))]
    [:ul.pagination.center
     (when-not (= 1 (count new-list))
       [:div.btn-group
        (map (fn [[label _page]]
               [:li  {:className (if (= page _page)
                                   "active"
                                   "waves-effect"
                                   )
                      :style {:width "50px"}
                      :key (str label _page)
                      :on-click (partial
                                 callback
                                 _page)}
                [:span
                 (if (or (= ">>" label)
                         (= "<<" label))
                   label
                   (+ 1 label))]]
               ) new-list)])]))

(defn home-page []
  [:div
   [pagination 10 1000 (fn []) 10]
   [:div ]])

(defn about-page []
  [:div [:h2 "About crud"]
   [:div [:a {:href "/"} "go to the home page"]]])

;; -------------------------
;; Routes

(def page (atom #'home-page))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))

(secretary/defroute "/about" []
  (reset! page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
