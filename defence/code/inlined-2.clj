(defn make-sidebar []
  (let [post-ids (fetch (PostIdsRequest))
        fetched-results (map (fn [id] [id (fetch (ReadCountRequest id) (CreationDateRequest id))]) post-ids)
        read-count-results (map (fn [res] [(first res) (first (second res))]))
        creation-date-results (map (fn [res] [(first res) (second (second res))]))
        most-read (take 5 (sort-by second read-count-results))
        most-recent (take 5 (sort-by second creation-date-results))]
    [(map first most-read)
     (map first most-recent)]))

(defn article [id]
  (let [sidebar (make-sidebar)
        content (fetch (ContentRequest id))
        header (fetch (LinksRequest))]
    (make-page content sidebar header)))
