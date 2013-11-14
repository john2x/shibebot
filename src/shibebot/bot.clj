(ns shibebot.bot
  (:require [clojure.string :as string]
            [reddit])
  (:use [shibebot.core]
        [clojure.set :only [intersection]]
        [opennlp nlp treebank]))

(def get-sentences (make-sentence-detector "opennlp_models/en-sent.bin"))

(defonce ^:const USERNAME "shibebote")

(defn suitable-sentence? [sentence]
  (let [s (string/lower-case (string/replace sentence #"[^a-zA-Z\s]" ""))
        words (string/split s #" ")
        username (re-pattern (str "/u/" USERNAME))]
    (if (or (re-find username s)
            (and (< (count words) 4)
                 (some #(re-find (re-pattern (str "^" % " ")) s) PREDICATES)
                 (string/blank? (re-find #"\?" s))))
      true
      false)))

(defn- extract-suitable-sentences [comment-body]
  "Returns a coll of suitable sentences from comment-body."
  (if (> (count comment-body) 140)
    []  ;; ignore comments that are too long
    (let [sentences (get-sentences comment-body)
          suitable-sentences (filter suitable-sentence? sentences)]
      (if (not-empty suitable-sentences)
        (vec (map #(string/replace %
                     (re-pattern (str "/u/" USERNAME)) "")  ;; remove /u/shibebote from the sentences
                  suitable-sentences))
        []))))

(defn suitable-comment? [comm]
  "Determine if a reddit comment is a suitable candidate
  for a shibe reply. "
  (not-empty (extract-suitable-sentences (:body comm))))

(defn run-bot []
  (reddit/login! USERNAME PASSWORD)
  (reddit/set-user-agent! "john2x/shibebot")
  (let [comments #(str % "comments/")]  ;; this should have been in reddit ns
    (->> "all" reddit/subreddit comments reddit/new-items
         (filter #(suitable-comment? %))
         (map #(-> % :body extract-suitable-sentences first shibify))
         (map println)
         dorun)))

;; (run-bot)
