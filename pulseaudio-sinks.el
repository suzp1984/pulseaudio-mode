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
(require 'cl)

(defgroup pulseaudio-sinks nil
  "Pulseaudio sinks management"
  :tag "Pulseaudio sinks"
  :version "0.1"
  :group 'pulseaudio)

(defcustom pulseaudio-sink-volume-step 5
  "define step when increase or decrease the volume of pulseaudio"
  :type 'integer
  :group 'pulseaudio-sinks)

(defvar pulseaudio-sinks-buffer-name "*Pulseaudio sinks*"
  "Pulseaudio sinks buffer name.")

(defvar pa-sinks-ewoc nil
  "The ewoc container of pulseaudio sinks.")

(defvar pulseaudio-sinks-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'pulseaudio-sinks-suspend)
    (define-key map (kbd "q") 'pulseaudio-quit-window)
    (define-key map (kbd "f") 'pulseaudio-sinks-refresh)
    (define-key map (kbd "-") 'pulseaudio-sinks-volume-less)
    (define-key map (kbd "+") 'pulseaudio-sinks-volume-more)
    (define-key map (kbd "n") 'pulseaudio-sinks-next)
    (define-key map (kbd "p") 'pulseaudio-sinks-prev)
    map)
  "pulseaudio-sinks-mode keymap")

(defvar pa-sinks-list nil
  "a list of all sinks")

(defvar pa-sinks-parser nil)

(defface pa-sinks-title-face 
  '((t (:foreground "BLUE" :weight bold)))
  "Default sinks title face.")

(defface pa-sinks-face-1 
  '((t (:foreground "GREEN" :weight bold)))
  "Pulseaduio face level 1")

(defface pa-sinks-face-2
  '((t (:foreground "YELLOW" :weight bold)))
  "Pulseaudio face level 2")

(defface pa-sinks-face-3
  '((t (:foreground "RED" :weight bold)))
  "Pulseaudio face level 3")

(defun pulseaudio-quit-window (&optional kill-buffer)
  "Bury the buffer and delete this window."
  (interactive "P")
  (quit-window kill-buffer (selected-window)))

(defun pa-ewoc-pp (object)
  "Pretty printer of ewoc."
  (let* ((beg (point))
         (num (plist-get object :num))
         (state (plist-get object :state))
         (name (plist-get object :name))
         (description (plist-get object :description))
         (driver (plist-get object :driver))
         (sample-specification (plist-get object :sample-specification))
         (channel-map (plist-get object :channel-map))
         (owner-module (plist-get object :owner-module))
         (mute (plist-get object :mute))
         (volume (plist-get object :volume))
         (base-volume (plist-get object :base-volume))
         (volume-value (car-safe (reverse base-volume)))
         (monitor-source (plist-get object :monitor-source))
         (latency (plist-get object :latency))
         (flags (plist-get object :flags))
         (properties (plist-get object :properties))
         (ports (plist-get object :ports))
         (active-port (plist-get object :active-port))
         (formats (plist-get object :formats)))
    (plist-put object :position (point))
    (insert (concat "Sink #" num))
    (insert "\n\t")
    (insert (concat (propertize "State: " 'face 'pa-sinks-title-face) (propertize state 'face 'pa-sinks-face-1)))
    (insert "\n\t")
    (insert (concat "Description: " (propertize description 'face 'pa-sinks-face-2)))
    (insert "\n\t")
    (insert (concat "Name: " (propertize name 'face 'pa-sinks-face-1)))
    (insert "\n\t")
    (insert (concat "Driver: " (propertize driver 'face 'pa-sinks-face-2)))
    (insert "\n\t")
    (insert (concat "Sample Specification: " (propertize sample-specification 'face 'pa-sinks-face-1)))
    (insert "\n\t")
    (insert (concat "Channel map: " (propertize channel-map 'face 'pa-sinks-face-1)))
    (insert "\n\t")
    (insert (concat "Owner Module: " (propertize owner-module 'face 'pa-sinks-face-2)))
    (insert "\n\t")
    (insert (concat "Mute: " (propertize mute 'face 'pa-sinks-face-1)))
    (insert "\n\t")
    (insert (concat "Volume: "))
    (dolist (item (reverse volume))
      (insert (propertize item 'face 'pa-sinks-face-2))
      (insert "\n\t\t\t"))
    (let ((value (car (reverse volume))))
      (when (string-match "\\([0-9]+\\)%" value)
        (setq value (match-string 1 value))
        (insert (concat "[" value "]: " 
                        (propertize (make-string (string-to-int value) ?#)
                                    'face 'pa-sinks-face-2)))))
    (insert "\n\t")
    (let ((value (car (cdr-safe base-volume))))
      (when (string-match "\\([0-9]+\\)%" value)
        (setq value (match-string 1 value))
        (insert (concat "Base Volume(" value "%): " 
                        (propertize (make-string (string-to-int value) ?#) 'face 'pa-sinks-face-1)))
        (message "volume: %s" value)))
    ;;(insert (concat "Base Volume: " "####"))
    (insert "\n\t")
    (insert (concat "Monitor Source: " (propertize monitor-source 'face 'pa-sinks-face-1)))
    (insert "\n\t")
    (insert (concat "Latency: " (propertize latency 'face 'pa-sinks-face-2)))
    (insert "\n\t")
    (insert (concat "Flags: " (propertize flags 'face 'pa-sinks-face-1)))
    (insert "\n\t")
    (insert (concat "Properties: "))
    (dolist (item (reverse properties))
      (insert (propertize item 'face 'pa-sinks-face-2))
      (insert "\n\t\t\t"))
    (insert "\n\t")
    (insert (concat "Ports: "))
    (dolist (item (reverse ports))
      (insert (propertize item 'face 'pa-sinks-face-1))
      (insert "\n\t\t\t"))
    (insert "\n\t")
    (insert (concat "Active Port: " (propertize active-port 'face 'pa-sinks-face-2)))
    (insert "\n\t")
    (insert (concat "Formats: " (propertize formats 'face 'pa-sinks-face-1)))
    (insert "\n")
  ;;  (put-text-property beg (point) 'read-only t)
    (put-text-property beg (point) 'front-sticky t)
    (put-text-property beg (point) 'rear-nonsticky t)))

(defun pulseaudio-sinks-suspend (node)
  "Suspend the sink"
  (interactive (list (ewoc-locate pa-sinks-ewoc)))
  ;;(message "%s" (plist-get (ewoc-data (ewoc-locate pa-sinks-ewoc)) :num))
  (let* ((data (ewoc-data node))
         (num (plist-get data :num))
         (state (plist-get data :state))
         (line (line-number-at-pos)))
    (message "%s : %s" num state)
    (if (string-match "RUNNING" state)
        (shell-command (concat "pactl suspend-sink " num " 1"))
      (shell-command (concat "pactl suspend-sink " num " 0"))
      )
    (pa-sinks-refresh)
    (goto-line line))
  )

(defun pulseaudio-sinks-refresh ()
  "Refresh the Pusleaudio Sinks buffer."
  (interactive)
  (let ((line (line-number-at-pos)))
    (pa-sinks-refresh)
    (goto-line line)))

(defun pulseaudio-sinks-next ()
  "move cursor to next sink"
  (interactive)
  (let* ((node (ewoc-locate pa-sinks-ewoc))
         position
         next-node)
    (when node 
      (setq next-node (ewoc-next pa-sinks-ewoc node))
      (unless next-node
        (while node
          (setq next-node node)
          (setq node (ewoc-prev pa-sinks-ewoc node))))
      (setq position (plist-get (ewoc-data next-node) :position))
      (goto-char position)))
  )

(defun pulseaudio-sinks-prev ()
  "move cursor to pre sink"
  (interactive)
  (let* ((node (ewoc-locate pa-sinks-ewoc))
         position
         pre-node)
    (when node
      (setq pre-node (ewoc-prev pa-sinks-ewoc node))
      (unless pre-node
        (while node
          (setq pre-node node)
          (setq node (ewoc-next pa-sinks-ewoc node)))
        )
      (setq position (plist-get (ewoc-data pre-node) :position))
      (goto-char position))))

(defun pulseaudio-sinks-volume-less (node)
  "Less the volume of a sink."
  (interactive (list (ewoc-locate pa-sinks-ewoc)))
  (let* ((data (ewoc-data node))
         (volume (car (reverse (plist-get data :volume))))
         (num (plist-get data :num))
         (line (line-number-at-pos))
         value)
    (when (string-match "\\([0-9]+\\)%" volume)
      (setq value (string-to-int (match-string 1 volume)))
      (setq value (- value pulseaudio-sink-volume-step))
      (shell-command (concat "pactl set-sink-volume " num " " 
                             (int-to-string value) "%"))
      (message "%s" (concat "pactl set-sink-volume " num " " 
                             (int-to-string value) "%"))
      (goto-line line)
      )
    )
  (pa-sinks-refresh))

(defun pulseaudio-sinks-volume-more (node)
  "Increase the volume of a sink."
  (interactive (list (ewoc-locate pa-sinks-ewoc)))
  (let* ((data (ewoc-data node))
         (volume (car (reverse (plist-get data :volume))))
         (num (plist-get data :num))
         (line (line-number-at-pos))
         value)
    (when (string-match "\\([0-9]+\\)%" volume)
      (setq value (string-to-int (match-string 1 volume)))
      (setq value (+ value pulseaudio-sink-volume-step))
      (shell-command (concat "pactl set-sink-volume " num " " 
                             (int-to-string value) "%"))
      (message "%s" (concat "pactl set-sink-volume " num " " 
                            (int-to-string value) "%"))
      (goto-line line)
      )
    )
  (pa-sinks-refresh))

;;;###autoload
(define-derived-mode pulseaudio-sinks-mode fundamental-mode "Pulseaudio Sinks"
  "Major mode for Pulseaudio Sinks management."
  (make-local-variable 'pa-sinks-ewoc)
  (unless pa-sinks-ewoc
    (setq pa-sinks-ewoc
          (ewoc-create 'pa-ewoc-pp 
                       (substitute-command-keys "\n\\{pulseaudio-sinks-mode-map}") 
                       "---"))
    (goto-char (point-max))
    ;;(put-text-property (point-min) (point) 'read-only t)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'rear-nonsticky t)))
  )

(defun pa-sinks-refresh ()
  "Refresh all sinks's status"
  ;;(erase-buffer)
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
  ;; delete all items
  (let ((node (ewoc-nth pa-sinks-ewoc 0)))
    (while node 
      (ewoc-delete pa-sinks-ewoc node)
      (setq node (ewoc-nth pa-sinks-ewoc 0))))
  (dolist (sink pa-sinks-list)
    (ewoc-enter-last pa-sinks-ewoc sink)))

(defmacro def-sinks-cond (regexp-str state line)
  (list `(string-match ,regexp-str ,line)
        `(let ((event (cons :value (match-string 1 line))))
           (fsm-update pa-sinks-parser ,state
                       (fsm-get-state-data pa-sinks-parser) nil)
           (fsm-send-sync pa-sinks-parser event))))

;;;###autoload
(defun list-pulseaudio-sinks ()
  "Display all pulseaudio sinks"
  (interactive)
  (let ((buffer (get-buffer-create 
                 (or pulseaudio-sinks-buffer-name "*PulseAudio Sinks*"))))
    (with-current-buffer buffer
      (unless (equal major-mode 'pulseaudio-sinks-mode)
        (pulseaudio-sinks-mode))
      (pa-sinks-refresh))
    (pop-to-buffer buffer)))

(provide 'pulseaudio-sinks)
;;; pulseaudio-sinks.el ends here
