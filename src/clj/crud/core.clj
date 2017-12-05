(ns crud.core)


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
