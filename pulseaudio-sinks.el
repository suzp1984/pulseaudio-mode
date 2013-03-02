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

(defvar pa-sinks-parser nil)

(defun pa-ewoc-pp (object)
  "Pretty printer of ewoc."
  )

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
    ))

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
