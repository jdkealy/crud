(ns crud.core)


(defn concat-memo [current _new]
  (let [oldq (:query current)
        oldi (:inputs current)
        newq (:query _new)
        newi (:inputs _new)
        _all {:query {:find (if-let [_find (:find newq)]
                     (vec (concat (:find oldq) _find))
                     (:find oldq))
                      :in (if-let [_in (:in newq)]
                            (vec (concat (:in oldq) _in))
                            (:in oldq))
                      :where (if-let [_where (:where newq)]
                               (vec (concat (:where oldq) _where))
                               (:where oldq)) }
              :inputs (vec (concat oldi newi))}]
    _all))

(defn concat-query [inputs]
  (vec (concat [(:query inputs)] (:inputs inputs))))


(defn reduce-concat-query [input config initial-state & [{:keys [sort-by]}]]
  (concat-query
   (reduce (fn [memo item]
             (let [_key item
                   _val (get input _key)]
               (concat-memo memo (config _key _val)))
             ) initial-state (if sort-by
                               (sort-by (keys input))
                               (keys input)) )))


(defn paginate-items [args _items]
  (let [pp (get args :pp 25 )
        pp (or pp 25)
        page-num (get args :page 0)
        page-num (or page-num 0)
        map-func (get args :map-func)
        sort-idx (get args :sort-idx 0)
        sort-key (get args :sort-key)
        reverse?  (if (= :desc (get args :order))
                    true
                    false)
        offset (* page-num pp)
        items (sort-by (fn [items]
                         (if sort-key
                           (get items sort-key)
                           (clojure.string/lower-case (get items sort-idx)))
                         ) _items)
        items (if reverse?
                (reverse items)
                items)
        offset (* page-num pp)
        items (take pp (drop offset items ))
        items (if map-func
                (doall (map map-func items))
                items)]
    {:items items
     :total (count _items)}))


(comment
  (paginate-items {:map-fn (fn [e]
                             e
                             )} [[1]])
  )
