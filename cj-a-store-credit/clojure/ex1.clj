(ns ex1
  (:import (java.util.concurrent LinkedBlockingQueue))
  (:import (java.io FileReader BufferedReader)))

(defn calc [vs q]
  (let [result (reduce + vs)]
    ;; (println result)
    (. q put result)
    result))

(defn find-match [t pl]
  (let [fp (first pl) ps (rest pl)
        remainder (seq (drop-while #(not (= (+ fp %) t)) ps))]
    (if (not (nil? remainder))
        [fp (first remainder)]
        (recur t ps))))

(defn process-input [bo3]
    (let [[l1 l2 l3] bo3
          total (. Integer parseInt l1)
          prices (map #(. Integer parseInt %) (re-seq #"\d+" l3))
          solution (find-match total prices)]
      (println "total   : " total)
      (println "prices  : " prices)
      (println "solution: " solution)))

(defn -main [path]
  (let [oq (new LinkedBlockingQueue)
        _fr (new FileReader path)
        br (rest (line-seq (new BufferedReader _fr)))]
    (doall (map process-input (partition 3 br))))
    (shutdown-agents)
  )
