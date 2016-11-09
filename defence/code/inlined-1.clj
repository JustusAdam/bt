(defn make-sidebar []
  (let [post-ids (fetch (PostIdsRequest))
        most-read (take 5 (sort-by second (map (fn [id] [id (fetch (ReadCountRequest id))]) post-ids)))
        most-recent (take 5 (sort-by second (map (fn [id] [id (fetch (CreationDateRequest id))]) post-ids)))]
    [(map first most-read)
     (map first most-recent)]))

(defn article [id]
  (let [sidebar (make-sidebar)
        content (fetch (ContentRequest id))
        header (fetch (LinksRequest))]
    (make-page content sidebar header)))
