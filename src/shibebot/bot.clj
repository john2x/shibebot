(ns shibebot.bot
  (:require [clojure.string :as string]
            [clojure.set :only intersection]
            [reddit])
  (:use [shibebot.core]
        [opennlp nlp treebank]))

(def get-sentences (make-sentence-detector "opennlp_models/en-sent.bin"))

(defonce ^:const USERNAME "shibebote")

(reddit/login! USERNAME PASSWORD)
(reddit/set-user-agent! "john2x/shibebote")

(defn suitable-sentence? [sentence]
  (let [s (string/replace sentence #"[^a-zA-Z\s]" "")
        words (string/split (string/lower-case s) #" ")
        username (re-pattern USERNAME)]
    (if (or (re-find (re-pattern USERNAME) s)
            (not-empty (clojure.set/intersection (set words) (set PREDICATES))))
      true
      false)))

(defn suitable-comment? [comment-body]
  "Determine if a reddit comment is a suitable candidate
  for a shibe reply. Returns a coll of suitable sentences
  if the comment is suitable. Otherwise, returns nil."
  (let [sentences (get-sentences comment-body)
        suitable-sentences (filter suitable-sentence? sentences)]
    (if (not-empty suitable-sentences)
      (vec (map #(string/replace %
                   (re-pattern (str "/u/" USERNAME)) "")  ;; remove /u/shibebote from the sentences
                suitable-sentences))
      nil)))

(defn run-bot []
  nil)

