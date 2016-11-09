(defn all-with-read-count []
  (map (fn [id] [id (fetch (ReadCountRequest id))]) (fetch (PostIdsRequest))))

(defn all-with-creation-date []
  (map (fn [id] [id (fetch (CreationDateRequest id))]) (fetch (PostIdsRequest))))

(defn make-sidebar []
  (let [most-read (take 5 (sort-by second (all-with-read-count)))
        most-recent (take 5 (sort-by second (all-with-creation-date)))]
    [(map first most-read)
     (map first most-recent)]))

(defn article [id]
  (let [sidebar (make-sidebar)
        content (fetch (ContentRequest id))
        header (fetch (LinksRequest))]
    (make-page content sidebar header)))
