(ns shibebot.core
  (:require [clojure.string :as string]
            [clojure.data.json :as json]))

;; API key generated from http://words.bighugelabs.com/api.php
(defonce ^:const BHT-API "http://words.bighugelabs.com/api/2/your-api-key/%s/json")
(defonce ^:const TEMPLATE
  [[nil   0 nil nil nil   6]
   [nil nil nil   1 nil nil]
   [  2 nil   7 nil nil nil]
   [nil nil nil nil   3 nil]
   [nil   5 nil   9 nil nil]
   [nil nil   4 nil nil   8]])

(def ^:const FILLERS
  ["wow" "shibe" "doge" "bot", "randum" "bote"])

(def ^:const PREDICATES
  ["such" "so" "much" "very" "much" "very" "so"])

(defn- rand-el [xs]
  (xs (rand-int (count xs))))

(defn- remove-predicates [s]
  (reduce #(string/replace %1 (first %2) (second %2))
          s
          (zipmap (map re-pattern PREDICATES)
                  (map (constantly "") PREDICATES))))

(defn- get-synonyms
  ([words] (get-synonyms words 5))
  ([words limit]
    (let [word-syns (atom {})]
      (swap! word-syns into (concat
        (for [word words
              :let [resp (-> (slurp (format BHT-API word))
                             (json/read-str :key-fn keyword))
                    syns (subvec (:syn (first (vals resp))) 0 limit)]]
          [word (cons word syns)])))
      @word-syns)))

(defn- random-phrases [words]
  (let [phrases (atom [])]
    (swap! phrases into (concat
      (for [word words
            :let [predicate (rand-el PREDICATES)]]
        (str predicate " " word))))
    @phrases))

(defn- random-fillers [n]
  (let [fillers (atom [])]
    (swap! fillers into (concat
      (for [x (range n)
            :let [predicate (rand-el PREDICATES)
                  filler (rand-el FILLERS)
                  wow-only? (= (rand-int 4) 0)]]
        (if wow-only?
          "wow"
          (str predicate " " filler)))))
    @fillers))

(defn shibe-phrases [seed n]
  "Return a collection of at most n shibe phrases (depends
  on the number of synonyms found for the seed text)
  based on seed text"
  (let [seeds (-> seed
                  remove-predicates
                  string/trim
                  (string/split #" ")
                  vec)
        syns (reduce into (vals (get-synonyms seeds)))
        phrases (random-phrases syns)
        fillers (random-fillers (count phrases))
        start (rand-int (count phrases))
        end (+ start n)
        phrases (set (interleave phrases fillers))
        shibe (-> phrases shuffle vec
                  (subvec start (min end (count phrases))))]
    shibe))

(defn- random-spaces []
  (string/join (repeat (+ 6 (rand-int 6)) " ")))

(defn- row->str [row]
  (reduce str "    " (map #(cond (nil? %) (random-spaces)
                                 (number? %) ""
                                 :else %) row)))

(defn- grid->str [grid]
  (string/join "\n" (map row->str grid)))

(defn render-shibe [phrases grid]
  "Render a collection of shibe phrases onto a grid"
  (let [rendered-grid (map (partial replace phrases) grid)
        text (grid->str rendered-grid)] 
    text))

(defn shibify [seed-text]
  "Generate shibe text from seed-text"
  (render-shibe (shibe-phrases seed-text 10) TEMPLATE))

