(ns random-test.core
  (:require [clojure.math.numeric-tower :as math]
            )
  (:gen-class)
  )

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

(defn average [num-seq]
  (/ (apply + num-seq) (count num-seq)))

(defn standard-deviation [num-seq]
  (let [avg (average num-seq)]
    (->> (map (fn [x]
                (let [n (math/abs (- x avg))]
                  (* n n)
                  )
                ) num-seq)
         average
         math/sqrt
         )
    )
  )

(defn print-distribution [m rang]
  (println "Random numbers: " (apply sorted-map (-> (seq m) sort flatten)))
  (when (< (count m) rang)
    (let [nums (set (keys m))]
      (println (- rang (count m)) "numbers absent: " (filter (complement nums) (range rang)))
      )
    )
  (println "Stand deviation: " (standard-deviation (vals m)))
  )

(defn -main [& args]
  (let [times (Integer/valueOf (or (first args) 20))
        rang (Integer/valueOf (or (second args) 10))]
    (println (format "Generating %d random integers between 0(inclusive) and %d(exclusive)" times rang))
    (let [random-seq (gen-random-seq times rang)]
      ; (println "Random sequence: " random-seq)
      (-> (compute-distribution random-seq)
          (#(print-distribution % rang))
          )
      )
    )
  )

