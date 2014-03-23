
(ns clojure-mail.core
  (:require [clojure-mail.parser :refer [html->text]])
  (:import  [java.util Properties]
            [javax.mail.search FlagTerm]
            [java.io FileInputStream File]
            [javax.mail.internet MimeMessage
                                 MimeMultipart
                                 InternetAddress]
            [javax.mail Session
                        Store
                        Folder
                        Message
                        Flags
                        Flags$Flag]))

;; Authentication
;; ***********************************************

(def settings (ref {:email nil :pass nil}))

(defn auth! [email pass]
  (dosync
    (ref-set settings
      {:email email :pass pass})))

;; Gmail
;; ***********************************************

(def gmail
  {:protocol "imaps"
   :server   "imap.gmail.com"})

(def gmail-folder-names
  {:inbox "INBOX"
   :all   "[Gmail]/All Mail"
   :sent  "[Gmail]/Sent Mail"
   :spam  "[Gmail]/Spam"})

;; Mail store
;; *******************************************************

(defn as-properties
  [m]
  (let [p (Properties.)]
    (doseq [[k v] m]
      (.setProperty p (str k) (str v)))
        p))

(defn store
  "A store models a message store and its access protocol,
   for storing and retrieving messages"
  [protocol server email pass]
  (let [p (as-properties [["mail.store.protocol" protocol]])]
    (try
      (doto (.getStore (Session/getDefaultInstance p) protocol)
        (.connect server email pass))
      (catch javax.mail.AuthenticationFailedException e
        (format "Invalid credentials %s : %s" email pass)))))

(defn gmail-store
  "Generates an email store which allows us access to our Gmail inbox"
  [email password]
  (let [{:keys [protocol server]} gmail]
    (store protocol server email password)))

(defonce ^:dynamic *store* nil)

(defmacro with-store
  "Takes a store which has been connected, and binds to to *store* within the
  scope of the form.

  **Usage:**

   user> (with-store (gmail-store \"username@gmail.com\" \"password\")
           (read-messages :inbox 5))
   ;=> "
  [s & body]
  `(binding [*store* ~s]
     ~@body))

(defn connected?
  "Returns true if a connection is established"
  ([] (connected? *store*))
  ([^com.sun.mail.imap.IMAPStore s]
     (.isConnected s)))

(defn close-store
  "Close an open IMAP store connection"
  ([] (close-store *store*))
  ([s] (.close s)))

(defn get-default-folder
  ^{:doc "Returns a Folder object that represents the 'root' of the default
          namespace presented to the user by the Store."}
  ([] (get-default-folder *store*))
  ([^com.sun.mail.imap.IMAPStore s]
     (.getDefaultFolder s)))

(defn get-folder
  "Return the Folder object corresponding to the given name."
  ([name] (get-folder *store* name))
  ([^com.sun.mail.imap.IMAPStore s name]
     (.getFolder s name)))


;; Message parser
;; *********************************************************
;; Utilities for parsing email messages

(defn mime-type
  "Determine the function to call to get the body text of a message"
  [type]
  (let [infered-type
         (clojure.string/lower-case
           (first (clojure.string/split type #"[;]")))]
  (condp = infered-type
    "multipart/alternative" :multipart
    "text/html" :html
    "text/plain" :plain
    (str "unexpected type, \"" type \"))))

(defn to
  "Returns a sequence of receivers"
  [m]
  (map str
    (.getRecipients m javax.mail.Message$RecipientType/TO)))

(defn from
  [m]
  (.getFrom m))

(defn subject
  "Fetch the subject of a mail message"
  [m]
  (.getSubject m))

(defn sender
  "Extract the message sender"
  [m]
  (.toString
   (.getSender m)))

;; Dates
;; *********************************************************

(defn date-sent
  "Return the date a mail message was sent"
  [m]
  (.toString
    (.getSentDate m)))

(defn date-recieved
  "Return the date a message was recieved"
  [m]
  (.toString
    (.getReceivedDate m)))

;; Flags
;; *********************************************************

(defn flags
  [m]
  (.getFlags m))

(defn content-type [m]
  (let [type (.getContentType m)]
    type))

(defn has-flag?
  [message flag]
  (let [f (flags message)]
    (boolean
      (.contains f flag))))

(defn user-flags [message]
  (let [flags (flags message)]
    (.getUserFlags flags)))

(defn read?
  "Checks if this message has been read"
  [message]
  (has-flag? message "SEEN"))

(defn answered?
  "Check if the message has an answered flag"
  [message]
  (has-flag? message "ANSWERED"))

(defn recent?
  [message]
  (has-flag? message "RECENT"))

(defn in-reply-to [m]
  (.getInReplyTo m))

(defn message-id [m]
  (.getMessageID m))

(defn encoding [m]
  (.getEncoding m))

(defn get-content [m]
  (.getContent m))

(defn message-headers
  "Returns all the headers from a message"
  [^com.sun.mail.imap.IMAPMessage msg]
  (let [headers (.getAllHeaders msg)
        results (enumeration-seq headers)]
    (into {}
      (map #(vector (.getName %) (.getValue %)) results))))

(defn- multipart? [m]
  "Returns true if a message is a multipart email"
  (.startsWith (content-type m) "multipart"))

(defn- read-multi [mime-multi-part]
  (let [count (.getCount mime-multi-part)]
    (for [part (map #(.getBodyPart mime-multi-part %) (range count))]
      (if (multipart? part)
        (.getContent part)
        part))))

(defn- message-parts
  [^javax.mail.internet.MimeMultipart msg]
  (if (multipart? msg)
    (read-multi (get-content msg))))

(defn msg->map
  "Convert a mail message body into a Clojure map
   with content type and message contents"
  [msg]
  {:content-type (.getContentType msg)
   :body (.getContent msg)})

(defn message-body
  [^com.sun.mail.imap.IMAPMessage msg]
  "Read all the body content from a message
   If the message is multipart then a vector is
   returned containing each message
   [{:content-type \"TEXT\\PLAIN\" :body \"Foo\"}
    {:content-type \"TEXT\\HTML\"  :body \"Bar\"}]"
  [msg]
  (if (multipart? msg)
    (map msg->map (message-parts msg))
    (msg->map msg)))

;; Public API for working with messages
;; *********************************************************

(defmacro safe-get
  "try to perform an action else just return nil"
  [& body]
  `(try
    (do ~@body)
  (catch Exception e#
    nil)))

(defn read-message [msg]
  "Returns a workable map of the message content.
   This is the ultimate goal in extracting a message
   as a clojure map"
  (try
    {:to (safe-get (first (to msg)))
     :from (safe-get (sender msg))
     :subject (safe-get (subject msg))
     :sender (safe-get (sender msg))
     :date-sent (safe-get (date-sent msg))
     :date-recieved (safe-get (date-recieved msg))
     :multipart? (safe-get (multipart? msg))
     :content-type (safe-get (content-type msg))
     :body (safe-get (message-body msg)) }
  (catch Exception e {:error e})))

;; *********************************************************

(def sub-folder?
  "Check if a folder is a sub folder"
  (fn [folder]
    (if (= 0 (bit-and
               (.getType folder) Folder/HOLDS_FOLDERS))
      false
      true)))

(defn folders
  "Returns a seq of all IMAP folders including sub folders"
  ([] (folders *store* (.getDefaultFolder *store*)))
  ([store] (folders store (.getDefaultFolder store)))
  ([store f]
     (map
      #(cons (.getName %)
             (if (sub-folder? %)
               (folders store %)))
      (.list f))))

(def folder-permissions
  {:readonly Folder/READ_ONLY
   :readwrite Folder/READ_WRITE})

(defn open-folder
  ([folder-name perm-level] (open-folder *store* folder-name perm-level))
  ([store folder-name perm-level]
     (let [folder (get gmail-folder-names folder-name)
           root-folder (.getDefaultFolder store)
           found-folder (get-folder root-folder folder)]
       (doto found-folder (.open (get folder-permissions perm-level))))))

(defn message-count
  "Returns the number of messages in a folder"
  ([folder-name] (message-count *store* folder-name))
  ([store folder-name]
     (let [folder (open-folder folder-name :readonly)]
       (.getMessageCount folder))))

(defn all-messages
  "Given a store and folder returns all messages
   reversed so the newest messages come first"
  ([folder-name] (all-messages *store* folder-name))
  ([^com.sun.mail.imap.IMAPStore store folder-name]
     (let [folder (open-folder folder-name :readonly)]
       (->> (.getMessages folder)
            reverse))))

(defn unread-messages
  "Find unread messages"
  ([folder-name] (unread-messages *store* folder-name))
  ([^com.sun.mail.imap.IMAPStore store folder-name]
     (let [folder (open-folder folder-name :readonly)]
       (doall (map read-message
                   (.search folder
                            (FlagTerm. (Flags. Flags$Flag/SEEN) false)))))))

(defn mark-all-read
  "Mark all messages in folder as read"
  ([folder-name] (mark-all-read *store* folder-name))
  ([^com.sun.mail.imap.IMAPStore store folder-name]
     (let [folder (open-folder folder-name :readwrite)
           messages (.search folder (FlagTerm. (Flags. Flags$Flag/SEEN) false))]
       (doall (map #(.setFlags % (Flags. Flags$Flag/SEEN) true) messages))
       nil)))


;; Saving / reading messages to filesystem
;; *********************************************************

(defn write-msg-to-dir
  "Write one message to a file in dir. Filename will look like:

  <e1878b88-e02e-4750-b4a4-357cbd4ad0fb@xtinp2mta143.xt.local>

  These message files can be read by the read-mail-from-file function"
  [dir msg]
  (.writeTo msg (java.io.FileOutputStream.
                 (format "%s%s" dir (str (message-id msg))))))

(defn save-msgs-to-dir
  "Handy function that dumps out a batch of emails to disk"
  [dir msgs]
  (doseq [msg msgs]
    (write-msg-to-dir dir msg)))

(defn read-mail-from-file
  "read a downloaded mail message in the same format
   as you would find on the mail server. This can
   be used to read saved messages from text files
   and for parsing fixtures in tests etc"
  [path-to-message]
  (let [props (Session/getDefaultInstance (Properties.))
        msg (FileInputStream. (File. path-to-message))]
    (MimeMessage. props msg)))

;; Public API
;; *********************************************************

(defn read-messages
  "Get all messages from a users inbox"
  ([folder-name limit] (read-messages *store* folder-name limit))
  ([store folder-name limit]
     (let [folder (get gmail-folder-names folder-name)
           messages (take limit (all-messages store folder))]
       (doall
        (map #(read-message %)
             messages)))))

(defn inbox
  "Get n messages from your inbox"
  ([limit] (inbox *store* limit))
  ([store limit]
    (read-messages store :inbox limit)))
