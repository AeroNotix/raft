(in-package :raft)


(defgeneric append-entries (transport server-id server-address append-entry))

(defgeneric append-entries-response (transport server-id server-address append-entry-response))

(defgeneric request-vote (transport server-id server-address request-vote))

(defgeneric request-vote-response (transport server-id server-address request-vote-response))


(defclass transport () ())
