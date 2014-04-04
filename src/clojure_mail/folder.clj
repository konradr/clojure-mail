(ns clojure-mail.folder
  (:import [javax.mail Flags Flags$Flag]))

;; note that the get folder fn is part of the store namespace

(def ^:dynamic current-folder)

(defmacro with-folder [folder store & body]
  `(let [fd# (doto (.getFolder ~store ~folder) (.open Folder/READ_ONLY))]
     (binding [current-folder fd#]
       (do ~@body))))

(defn full-name [f]
  (.getFullName f))

(defn new-message-count [f]
  "Get number of new messages in folder f"
  (.getNewMessageCount f))

(defn message-count [f]
  "Get total number of messages in folder f"
  (.getMessageCount f))

(defn unread-message-count [f]
  "Get number of unread messages in folder f"
  (.getUnreadMessageCount f))

(defn get-message-by-uid [f id]
  (.getMessageByUID f id))

(defn get-message [f id]
  (.getMessage f id))

(defn get-uid [f m]
  (.getUID f m))

(defn delete-message [f id]
  (let [message (get-message-by-uid f id)]
    (.setFlag message Flags$Flag/DELETED true)))

(defn get-messages
  "Gets all messages from folder f"
  [folder]
  (.getMessages folder))

(defn search [f query]
  (.search f query))

(defn list-folders [f]
  "List all folders under folder f"
  (.list f))
