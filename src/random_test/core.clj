(ns random-test.core
  (:gen-class))

(defn gen-random-seq [times range]
  (repeatedly times #(rand-int range))
  )

(defn compute-distribution [random-seq]
  (reduce (fn [m i]
            (update m i #(inc (or % 0)))
            ) {} random-seq)
  )

(defn print-distribution [m]
  (println (apply sorted-map (sort (seq m))))
  )

(defn -main [& args]
  (let [times (Integer/valueOf (or (first args) 10))
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

