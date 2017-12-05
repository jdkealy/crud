(ns crud.pagination

  )


(defn cmp [page total callback pp]
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
