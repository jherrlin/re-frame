(ns re-frame.settings
  (:require
   [re-frame.interop :as interop]
   [re-frame.loggers :refer [console]]))

(def defaults
  {:loaded?             false
   :global-interceptors interop/empty-queue})

(def store
  (atom defaults))

(interop/on-load
 #(swap! store (fn [m] (assoc m :loaded? true))))

(defn loaded?
  []
  (:loaded? @store))

(defn reduce-to-queue [f interceptors]
  (reduce f interop/empty-queue interceptors))

(defn -replace-global-interceptor
  [global-interceptors interceptor]
  (reduce-to-queue
   (fn [ret existing-interceptor]
     (if (= (:id interceptor)
            (:id existing-interceptor))
       (do
         (when interop/debug-enabled?
           (when (not (loaded?))
             (console :warn "re-frame: replacing duplicate global interceptor id: " (:id interceptor))))
         (conj ret interceptor))
       (conj ret existing-interceptor)))
   global-interceptors))

(defn -sort-interceptors [interceptors]
  (let [first-inceptors     (->> interceptors
                                 (filter (comp neg-int? :order))
                                 (sort-by :order #(compare %2 %1)))
        unordered-inceptors (->> interceptors
                                 (filter (comp nil? :order)))
        last-inceptors      (->> interceptors
                                 (filter (comp pos-int? :order))
                                 (sort-by :order))]
    (reduce-to-queue
     (fn [ret interceptor]
       (conj ret interceptor))
     (concat first-inceptors unordered-inceptors last-inceptors))))

(defn reg-global-interceptor
  [{:keys [id] :as interceptor}]
  (swap! store update :global-interceptors
         (fn [global-interceptors]
           (let [ids (map :id global-interceptors)]
             (if (some #{id} ids)
               ;; If the id already exists we replace it in-place to maintain the ordering of
               ;; global interceptors esp during hot-code reloading in development.
               (-replace-global-interceptor global-interceptors interceptor)
               (->> (conj global-interceptors interceptor)
                    (-sort-interceptors)))))))

(defn get-global-interceptors
  []
  (:global-interceptors @store))

(defn clear-global-interceptors
  ([]
   (swap! store assoc :global-interceptors interop/empty-queue))
  ([id]
   (swap! store update :global-interceptors
          (fn [global-interceptors]
            (into interop/empty-queue (remove #(= id (:id %)) global-interceptors))))))
