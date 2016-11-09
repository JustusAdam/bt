(defn fetch-pages []
  (fetch (Pages.)))

(defn fetch-content [id]
  (fetch (Content. id)))

(defn fetch-post-ids []
  (fetch (PostIDs.)))

(defn fetch-creation-date [id]
  (fetch (CreationDate. id)))

(defn fetch-read-count [id]
  (fetch (ReadCount. id)))