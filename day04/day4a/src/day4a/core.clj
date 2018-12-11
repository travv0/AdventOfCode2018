(ns day4a.core
  (require [clojure.string :as str])
  (:gen-class))

(defn count-sleep* [log guard-id sleep-start-time counts]
  (if (empty? log)
    counts
    (let [entry (first log)]
      (cond (some #{\#} entry)
            (recur (rest log)
                   (Integer/parseInt (second (re-find #"#(\d+) " entry)))
                   sleep-start-time
                   counts)

            (str/includes? entry "falls asleep")
            (recur (rest log)
                   guard-id
                   (Integer/parseInt (second (re-find #":(\d\d)\]" entry)))
                   counts)

            :else
            (recur (rest log)
                   guard-id
                   nil
                   (reduce #(assoc-in %1
                                      [guard-id %2]
                                      (inc (get-in %1 [guard-id %2] 1)))
                           counts
                           (range sleep-start-time
                                  (Integer/parseInt (second (re-find #":(\d\d)\]" entry))))))))))

(defn most-minutes-id [counts]
  (let [sum-map #(+ %1 %3)]
    (:id (reduce-kv #(let [sleep (reduce-kv sum-map 0 %3)]
                       (if (< (:sleep %1) sleep)
                         {:id %2, :sleep sleep}
                         %1))
                    {:id 0, :sleep 0}
                    counts))))

(defn most-minutes-time [guard-counts]
  (:time (reduce-kv #(if (< (:sleep %1) %3)
                       {:time %2, :sleep %3}
                       %1)
                    {:time 0, :sleep 0}
                    guard-counts)))

(defn count-sleep [lines]
  (let [log (sort lines)]
    (count-sleep* log nil nil {})))

(defn -main [& args]
  (let [counts (count-sleep (line-seq (java.io.BufferedReader. *in*)))
        sleepiest-guard (most-minutes-id counts)
        sleepiest-minute (most-minutes-time (get counts sleepiest-guard))]
    (println (* sleepiest-guard sleepiest-minute))))
