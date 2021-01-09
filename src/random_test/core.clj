(ns random-test.core
  (:gen-class))

(defn gen-random-seq [times range]
  (repeatedly times #(rand-int range))
  )

(defn compute-distribution [random-seq]
  (reduce (fn [m i]
            (let [v (or (get m i) 0)]
              (assoc m i (inc v))
              )
            ) {} random-seq)
  )

(defn print-distribution [m]
  (println (apply sorted-map (-> (seq m) sort flatten)))
  )

(defn -main [& args]
  (let [times (Integer/valueOf (or (first args) 20))
        range (Integer/valueOf (or (second args) 10))]
    (println (format "Generating %d random integers between 0(inclusive) and %d(exclusive)" times range))
    (let [random-seq (gen-random-seq times range)]
      (println random-seq)
      (-> (compute-distribution random-seq)
          print-distribution
          )
      )
    )
  )

