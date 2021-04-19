;;; alerter.el --- Alerter partial integration for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 berquerant

;; Author: berquerant
;; Maintainer: berquerant
;; Package-Requires: ((cl-lib "1.0"))
;; Created: 18 Apr 2021
;; Version: 0.1.0
;; Keywords: alerter integration
;; URL: https://github.com/berquerant/emacs-alerter

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Please see README.md from the same repository for documentation.

;;; Code:

(require 'cl-lib)

(defgroup alerter nil
  "Alerter integration.")

(defcustom alerter-gen-uuid nil
  "UUID generator."
  :group 'alerter
  :type 'function
  :version "27.2")

(defun alerter--gen-uuid ()
  "Generate UUID by `alerter-gen-uuid'."
  (if alerter-gen-uuid
      (funcall alerter-gen-uuid)
    "00000000-0000-0000-0000-000000000000"))

(defcustom alerter-notification-process-name "alerter-notification"
  "Process name for notification."
  :group 'alerter
  :type 'string
  :version "27.2")

(defcustom alerter-notification-buffer-name (format "*%s*" alerter-notification-process-name)
  "Buffer name for notification."
  :group 'alerter
  :type 'string
  :version "27.2")

(defcustom alerter-notification-program-name "alerter"
  "Program name for notification."
  :group 'alerter
  :type 'string
  :version "27.2")

(defcustom alerter-notification-log-quiet nil
  "Do not output log to `alerter-notification-buffer-name' buffer if t."
  :group 'alerter
  :type 'boolean
  :version "27.2")

(defvar alerter-notification-program (executable-find alerter-notification-program-name)
  "Program for notification.
executable path of `alerter-notification-program-name'.")

(defun alerter--to-datetime (&optional time zone)
  "Convert TIME (timestamp) in ZONE (timezone) into string.
default TIME is now, ZONE is here."
  (format-time-string "%F %T"
                      (or time (current-time))
                      (or zone (current-time-zone))))

(defun alerter--to-timestamp (&optional time)
  "Convert TIME (time-string) into timestamp.
default TIME is now.  Time zone is here."
  (let ((ts (or (and time (apply #'encode-time (parse-time-string time)))
                (current-time))))
    (+ (* (car ts) (expt 2 16)) (cadr ts))))

(defun alerter--invoke-after (duration hook)
  "Invoke HOOK after DURATION seconds.
DURATION is not negative, HOOK has no arguments."
  (run-at-time (or (and (>= duration 0) duration) 0)
               nil hook))

(defun alerter-insert-to-notification-buffer (arg)
  "Insert ARG into `alerter-notification-buffer-name' buffer."
  (unless alerter-notification-log-quiet
    (with-current-buffer (get-buffer-create alerter-notification-buffer-name)
      (insert arg))))

;;;###autoload
(cl-defun alerter-alert (msg &key title subtitle actions drop-down-label sound app-icon content-image close-label timeout reply uuid hook)
  "Notify using alerter (https://github.com/vjeantet/alerter).
You will see MSG.  you can use keyword arguments of alerter.

  :title for -title (current-buffer@Emacs)
  :subtitle for -subtitle (nil)
  :sound for -sound (default)
  :actions for -actions, drop down list items (comma separated string), this and reply are exclusive
  :drop-down-label for -dropdownLabel (nil), you can specify this when use actions
  :close-label for -closeLabel (Close)
  :timeout for -timeout, notification is closed after TIMEOUT seconds, nil means notification is nerver closed by timeout
  :reply for -reply, placeholder to get reply from user, nil means reply is unavailable, this and actions are exclusive
  :app-icon for -appIcon (nil)
  :content-image for -contentImage (nil)
  :uuid for log (nil)
  :hook is a function (process output) called when notification finished (nil)

write result of notification process and logs to `alerter-notification-buffer-name' buffer."
  (if (not (or alerter-notification-program
               (setq alerter-notification-program (executable-find alerter-notification-program-name))))
      (progn
        (message "[alerter] I cannot notify because %s not found." alerter-notification-program-name)
        (return-from 'alerter-alert)))
  (let* ((argtuples
          `(("-message" . ,msg)
            ("-title" .  ,(or title
                              (format "%s@Emacs" (buffer-name (current-buffer)))))
            ("-subtitle" . ,subtitle)
            ("-sound" . ,(or sound "default"))
            ("-timeout" . ,(and timeout (>= timeout 0) (number-to-string timeout)))
            ("-actions" . ,actions)
            ("-dropdownLabel" . ,drop-down-label)
            ("-reply" . ,reply)
            ("-appIcon" . ,app-icon)
            ("-contentImage" . ,content-image)
            ("-sender" . "Emacs")
            ("-closeLabel" . ,(or close-label "Close"))))
         (args
          (loop for (k . v) in argtuples
                when (and v (> (length v) 0))
                collect k
                and collect (encode-coding-string v (keyboard-coding-system)))))
    (alerter-insert-to-notification-buffer (format "[alerter] %s %s %s (%s)\n"
                                                      (alerter--to-datetime)
                                                      alerter-notification-program
                                                      (mapconcat 'identity args " ")
                                                      (or uuid "UUID")))
    (let ((p (apply #'start-process
                    (append `(,alerter-notification-process-name
                              ,alerter-notification-buffer-name
                              ,alerter-notification-program)
                            args)))
          (h (or hook (lambda (proc output)))))
      (set-process-filter p `(lambda (proc output)
                               (,h proc output)
                               (alerter-insert-to-notification-buffer output)))
      p)))

;;;###autoload
(cl-defun alerter-alert-at-time (msg &key time title subtitle actions drop-down-label sound app-icon content-image close-label timeout reply uuid hook)
  "Notify MSG at TIME (time-string).
default TIME is now.
See `alerter-alert' for other options."
  (let* ((dt (or (and time (- (alerter--to-timestamp time) (alerter--to-timestamp)))
                 0))
         (uuid (alerter--gen-uuid))
         (log (format "[alerter] %s I will notify '%s' at %s (%d seconds later) (%s)"
                      (alerter--to-datetime)
                      msg
                      (or time "now")
                      dt
                      uuid)))
    (alerter--invoke-after dt `(lambda () (alerter-alert ,msg
                                                         :title ,title
                                                         :subtitle ,subtitle
                                                         :actions ,actions
                                                         :drop-down-label ,drop-down-label
                                                         :sound ,sound
                                                         :app-icon ,app-icon
                                                         :content-image ,content-image
                                                         :close-label ,close-label
                                                         :timeout ,timeout
                                                         :reply ,reply
                                                         :uuid ,uuid
                                                         :hook ',hook)))
    (message log)
    (alerter-insert-to-notification-buffer (format "%s\n" log))))

(provide 'alerter)
;;; alerter.el ends here
