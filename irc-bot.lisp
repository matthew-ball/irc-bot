
(ql:quickload 'cl-irc)

(defvar *connection* nil "IRC connection.")

(defparameter *default-server* "irc.freenode.net" "Default server")
(defparameter *default-port* 6667 "Default port")
(defparameter *default-nick-name* "ccl_bot" "Default nick name")
(defparameter *default-channels* '("##testing-au") "Default channels")

(defun join (channel) (irc:join *connection* channel))
(defun part (channel) (irc:part *connection* channel))
(defun private-message (destination string) (irc:privmsg *connection* destination string))
(defun stop (&optional (message "Bye!")) (irc:quit *connection* message))

(defun valid-message (string prefix &key space-allowed)
  (if (equal (search prefix string :test #'char-equal) 0)
      (and (or space-allowed (not (find #\Space string :start (length prefix)))) (length prefix))
      nil))

(defmacro alternative-forms (name)
  `(list (format nil "~A: " ,name)
		 (format nil "~A, " ,name)))

(defun strip-name (string &key (name *default-nick-name*) (final nil))
  (loop for i in (alternative-forms name)
	 do (let ((msg (valid-message string i :space-allowed t)))
		  (when msg
			(return-from strip-name (subseq string msg)))))
  (and (not final) string))

(defun process-message (message)
  (let ((msg (strip-name message)))
	(cond
	  (t (format nil "Sorry, I don't understand ~A" msg)))))

(defun private-message-handler (message)
  (let ((destination (if (string-equal (first (irc:arguments message)) *default-nick-name*)
						 (irc:source message)
						 (first (irc:arguments message))))
		(lookup (first (last (irc:arguments message)))))
	(private-message destination (process-message lookup))))

(defun start-bot ()
  (setf *connection* (irc:connect :nickname *default-nick-name*
								  :server *default-server*
								  :port *default-port*))
  (mapc #'join *default-channels*)
  (irc:add-hook *connection* 'irc:irc-privmsg-message 'private-message-handler)
  (irc:read-message-loop *connection*))
