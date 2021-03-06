;;; org-planning-info.el --- add planning info to org-ql views -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Mikhail Skorzhisnkii
;;
;; Author: Mikhail Skorzhisnkii <mskorzhinskiy@eml.cc>>
;; Maintainer: Mikhail Skorzhisnkii <mskorzhinskiy@eml.cc>
;; Created: December 27, 2020
;; Modified: December 27, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/rasmi/org-planning-info
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'org)
(require 'org-ql)

(defcustom org-ql-planning-formats '(" %s - %s » %03s »"
                                     "     %s     » %03s »"
                                     "     %s     »      »"
                                     "  %03s »"
                                     "       »")
  "Format strings for agenda entries.

Which formater will be used depends on scheduled cookie and
effort property. If scheduled property have an ending (e.g.
10:30-11:00), then it is an appointment with duration. If there
is no duration, but there is effort properties, then format for
appointment with duration will be used.

\"Appointment with duration\"       First argument is appointment
                                    start, second argument is
                                    appointment end and third is the
                                    time between these two timestamps.

\"Appointment with effort\"         First argument is appointment start,
                                    second is effort property.

\"Appointment\" and \"Only effort\" The only arguments are
                                    appointment start and effort."
  :group 'org-agenda
  :type '(list (string :tag "Appointment with duration")
               (string :tag "Appointment with effort")
               (string :tag "Appointment")
               (string :tag "Only effort")))

(defface org-ql-planning-overspent-face
  '((t (:slant italic :weight bold :inherit error)))
  "Face for overspent efforts in `org-ql-view' views."
  :group 'org-ql)

(defun org-ql-planning-get-time-position (line)
  "Get position of timed string in QL LINE format string."
  (interactive)
  (catch 'found
    (let ((it 0) (length (length line)))
      (while (< it length)
        (when (eq (get-text-property it 'face line) 'org-ql-view-due-date)
          (throw 'found it))
        (setq it (1+ it)))
      0)))

(defun org-ql-planning-get-clocked-minutes (pom)
  "Get clocked currently clocked time at POM."
  (interactive)
  (org-with-point-at pom
    (org-with-wide-buffer
     (if-let ((scheduled (org-entry-get pom "SCHEDULED"))
              (scheduled (ts-format (ts-parse scheduled))))
         (org-clock-sum-current-item scheduled)
       (org-clock-sum-current-item)))))

;;;###autoload
(defun org-ql-planning-get-effort-value-without-clocked (a)
  "Get overall effort value without clocked time of element A."
  (let* ((pom (plist-get (cadr a) :org-marker))
         (effort (or (org-entry-get pom "EFFORT")
                     0))
         (effort-minutes (org-duration-to-minutes effort))
         (clocked-minutes (org-ql-planning-get-clocked-minutes pom)))
    (- effort-minutes clocked-minutes)))

(defun org-ql-planning-do-format-line (func &rest args)
  "Advice function to insert time of the day for entries.

FUNC for original function and ARGS for original ARGS."
  (let* ((line (apply func args))
         (scheduled (get-text-property 0 'scheduled line))
         (effort-minutes (org-ql-planning-get-effort-value-without-clocked
                          (nth 0 args)))
         (start (org-timestamp-to-time scheduled))
         (end   (org-timestamp-to-time scheduled 'end))
         (duration-seconds (time-to-seconds (time-subtract end start)))
         (have-hour-ts (org-element-property :hour-start scheduled))
         (have-duration (not (= 0 duration-seconds)))
         (duration (format-seconds "%h:%02m" duration-seconds))
         (position (org-ql-planning-get-time-position line)))
    (let* ((prop-list (text-properties-at position line))
           (effort (cond ((not (get-text-property 0 'EFFORT line))
                          nil)
                         ((> 0 effort-minutes)
                          (plist-put prop-list
                                     'face 'org-ql-planning-overspent-face)
                          (org-duration-from-minutes (abs effort-minutes)))
                         (t
                          (org-duration-from-minutes effort-minutes))))
           (result
            (cond (have-duration
                   ;; If we have an appointment with duration, it's better to
                   ;; calculate duration from planning information rather then
                   ;; using effort value. Effort value in this case would
                   ;; useless, as effort value is way less precise.
                   (format (nth 0 org-ql-planning-formats)
                           (format-time-string "%H:%M" start)
                           (format-time-string "%H:%M" end)
                           duration))
                  ;; If we have both effort value and event start, don't bother
                  ;; writing full line, as effort value is something not really
                  ;; precise. Just write event start and expecatation.
                  ;;
                  ;; TODO: May be just mark entry somehow that this is an expcted
                  ;; end and calculate end?
                  ((and have-hour-ts effort)
                   (format (nth 1 org-ql-planning-formats)
                           (format-time-string "%H:%M" start)
                           effort))
                  ;; Only event start
                  (have-hour-ts
                   (format (nth 2 org-ql-planning-formats) (format-time-string "%H:%M" start)))
                  ;; Only effort value
                  (effort
                   (format (nth 3 org-ql-planning-formats) effort))
                  ;; Nothing at all
                  (t
                   (format (nth 4 org-ql-planning-formats))))))
      (if result
          (format "%s%s" (org-add-props result prop-list) line)
        line))))

;;;###autoload
(defun org-ql-planning-load ()
  "Install formating advice."
  (advice-add 'org-ql-view--format-element
              :around 'org-ql-planning-do-format-line))

;;;###autoload
(defun org-ql-planning-unload ()
  "Uninstall formatting advice."
  (advice-remove 'org-ql-view--format-element
                 'org-ql-planning-do-format-line))

(provide 'org-planning)
;;; org-planning.el ends here
