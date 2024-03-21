(require '[clojure.string :as str])
(require '[clojure.java.io :as io])

(defn timestamp-line? [line]
  (re-matches #"^(\d+:\d+:\d+,\d+)\s*-->\s*(\d+:\d+:\d+,\d+).*"
              line))

(defn timestamp->ms [ts]
  (let [[h m s ms] (->> (str/split ts #"[:,]")
                        (map #(Long/parseLong %)))]
    (+ ms
       (* 1000 s)
       (* 1000 60 m)
       (* 1000 60 60 h))))

(defn parse-timestamp-line [line]
  (when-let [[_ begin end] (timestamp-line? line)]
    [(timestamp->ms begin) (timestamp->ms end)]))

(defn pad-left
  ([string len] (pad-left string len " "))
  ([string len char]
   (let [string (str string)]
     (str (str/join (repeat (- len (count string)) char)) string))))

(defn format-duration [duration]
  (let [ms (int (/ (mod duration 1000) 1))
        s (int (mod (/ duration 1000) 60))
        m (int (mod (/ duration (* 1000 60)) 60))
        h (int (mod (/ duration (* 1000 60 60)) 60))]
    (str (pad-left h 2 "0") ":"
         (pad-left m 2 "0") ":"
         (pad-left s 2 "0") ","
         (pad-left ms 3 "0"))))

(let [[in-path delta] *command-line-args*
      out-path (str/replace "test.srt" #"\.srt$" "_out.srt")
      delta (parse-double (str delta))]
  (assert (and in-path (str/ends-with? in-path ".srt")) "INVALID PATH")
  (assert (number? delta) "INVALID DELTA")
  (println (str "Correcting " in-path " by " delta " seconds.") )
  (with-open [w (io/writer out-path :encoding "UTF-8")]
    (doseq [line (str/split-lines (slurp in-path))]
      (if-let [[begin end] (parse-timestamp-line line)]
        (.write w (str (format-duration (max 0 (+ begin (* 1000 delta))))
                       " --> "
                       (format-duration (max 0 (+ end (* 1000 delta))))))
        (.write w line))
      (.write w "\r\n")))
  (println "File written successfully to:" out-path))