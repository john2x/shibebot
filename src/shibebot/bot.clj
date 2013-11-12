(ns shibebot.bot
  (:require [clojure.string :as string]
            [shibebot.core :only shibe-phrases]
            [reddit.clj.core :as reddit]))

(defonce ^:const USERNAME "dogebot")
(defonce ^:const PASSWORD "xxx")
(defn find-comment-candidates []
  "Find comment candidates suitable for a shibe reply"
  (print "finding comment candidates...")
  nil)

(defn run-bot []
  nil)

