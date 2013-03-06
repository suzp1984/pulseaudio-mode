;;; pulseaudio-sinks.el --- pulseaudio sinks modules

;; Copyright (C) 2013  zxsu

;; Author: zxsu <suzp1984@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'ewoc)

(defgroup pulseaudio-sinks nil
  "Pulseaudio sinks mangement"
  :tag "Pulseaudio sinks"
  :version "0.1"
  :group 'pulseaudio)

(defvar pulseaudio-sinks-buffer-name "*Pulseaudio sinks*"
  "Pulseaudio sinks buffer name.")

(defvar pa-sinks-ewoc nil
  "The ewoc container of pulseaudio sinks.")

(defvar pulseaudio-sinks-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'pulseaudio-sinks-suspend)
    map)
  "pulseaudio-sinks-mode keymap")

(defvar pa-sinks-list nil
  "a list of all sinks")

(defvar pa-sinks-parser nil)

(defun pa-ewoc-pp (object)
  "Pretty printer of ewoc."
  (let ((beg (point))
        (num (plist-get object :num))
        (state (plist-get object :state))
        (name (plist-get object :name))
        (description (plist-get object :description))
        (driver (plist-get object :driver)))
    (insert (concat "Sink #" num))
    (insert "\n")
    (insert "\t")
    (insert (concat "State: " state))
    (insert "\n\t")
    (insert (concat "Description: " description))
    (insert "\n\t")
    (insert (concat "Name: " name))
    (put-text-property beg (point) 'read-only t)
    (put-text-property beg (point) 'front-sticky t)
    (put-text-property beg (point) 'rear-nonsticky t)))

(defun pulseaudio-sinks-suspend (&optional node)
  )

;;;###autoload
(define-derived-mode pulseaudio-sinks-mode fundamental-mode "Pulseaudio Sinks"
  "Major mode for Pulseaudio Sinks management."
  (make-local-variable 'pa-sinks-ewoc)
  (unless pa-sinks-ewoc
    (setq pa-sinks-ewoc
          (ewoc-create 'pa-ewoc-pp nil nil))
    (goto-char (point-max))
    (put-text-property (point-min) (point) 'read-only t)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'rear-nonsticky t)))
  )

(defun pa-sinks-refresh ()
  "Refresh all sinks's status"
  (let ((output (shell-command-to-string "pactl list sinks")))
    (setq pa-sinks-parser (start-pa-sinks-parser output))
    (dolist (line (split-string output "\n" t))
      (cond ((string-match "^Sink #\\([0-9]+\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-start
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*State:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-state
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Name:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-name
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Description:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-description
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Driver:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-driver
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Sample Specification:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-sample
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Channel Map:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-channel
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Owner Module:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-owner-module 
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Mute:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-mute
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Volume:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-volume
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Base Volume:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-base-volume
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Monitor Source:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-monitor-source
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Latency:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-latency
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Flags:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-flags
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Properties:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-properties
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Ports:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-ports
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Active Port:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-active-port
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            ((string-match "^[ \t]*Formats:[ \t]*\\(.*\\)" line)
             (let ((event (cons :value (match-string 1 line))))
               (fsm-update pa-sinks-parser :sinks-formats
                           (fsm-get-state-data pa-sinks-parser) nil)
               (fsm-send-sync pa-sinks-parser event)))
            (t 
             (let ((event (cons :value (trim-string line))))
               (fsm-send-sync pa-sinks-parser event)))
            ))
    (fsm-update pa-sinks-parser :sinks-start
                (fsm-get-state-data pa-sinks-parser) nil)
    )
  (dolist (sink pa-sinks-list)
    (ewoc-enter-last pa-sinks-ewoc sink)))

(defmacro def-sinks-cond (regexp-str state line)
  (list `(string-match ,regexp-str ,line)
        `(let ((event (cons :value (match-string 1 line))))
           (fsm-update pa-sinks-parser ,state
                       (fsm-get-state-data pa-sinks-parser) nil)
           (fsm-send-sync pa-sinks-parser event))))

(defun list-pulseaudio-sinks ()
  "Display all pulseaudio sinks"
  (interactive)
  (let ((buffer (get-buffer-create 
                 (or pulseaudio-sinks-buffer-name "*PulseAudio Sinks*"))))
    (with-current-buffer buffer
      (pulseaudio-sinks-mode)
      (pa-sinks-refresh))
    (pop-to-buffer buffer)))

(provide 'pulseaudio-sinks)
;;; pulseaudio-sinks.el ends here
