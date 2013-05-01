;;; pulseaudio-sink-inputs.el --- sink-inputs management modes

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

(defgroup pulseaudio-sink-inputs nil
  "PulseAudio sink-inputs management"
  :tag "Pulseaudio sink-inputs"
  :version "0.1"
  :group 'pulseaudio)

(defcustom pulseaudio-sink-inputs-volume-step 5
  "define step when increase or decrease the volume of pulseaudio"
  :type 'integer
  :group 'pulseaudio-sink-inputs)

(defvar pulseaudio-sink-inputs-buffer-name "*Pulseaudio sink-inputs*"
  "Pulseaudio sink-inputs buffer name.")

(defvar pa-sink-inputs-ewoc nil
  "The ewoc container of pulseaudio sink-inputs.")

(defvar pulseaudio-sink-inputs-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'pulseaudio-sink-inputs-suspend)
    (define-key map (kbd "q") 'pulseaudio-quit-window)
    (define-key map (kbd "f") 'pulseaudio-sink-inputs-refresh)
    (define-key map (kbd "-") 'pulseaudio-sink-inputs-volume-less)
    (define-key map (kbd "+") 'pulseaudio-sink-inputs-volume-more)
    (define-key map (kbd "n") 'pulseaudio-sink-inputs-next)
    (define-key map (kbd "p") 'pulseaudio-sink-inputs-prev)
    map)
  "pulseaudio-sink-inputs-mode keymap")

(defvar pa-sink-inputs-list nil
  "a list of all sinks")

(defvar pa-sink-inputs-parser nil)

(defun pa-sink-inputs-pp (object)
  "Pretty printer of ewoc"
  )

;;;###autoload
(define-derived-mode pulseaudio-sink-inputs-mode fundamental-mode "Pulseaudio sink inputs"
  "Major mode for Pulseaudio sink inputs management"
  (make-local-variable 'pa-sink-inputs-ewoc)
  (unless pa-sink-inputs-ewoc 
    (setq pa-sink-inputs-ewoc 
          (ewoc-create 'pa-sink-inputs-pp 
                       (substitute-command-keys "\n\\{pulseaudio-sinks-mode-map}")
                       "---"))
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'rear-nonsticky t))))

(defun pulseaudio-sink-inputs-refresh ()
  "Refresh all sink inputs status"
  (let ((output (shell-command-to-string "pactl list sink-inputs")))
    ))

;;;###autoload
(defun list-pulseaudio-sink-inputs ()
  "Display all pulseaudio sink inputs, and enter sink-inputs management panel."
  (interactive)
  (let ((buffer (get-buffer-create 
                 (or pulseaudio-sink-inputs-buffer-name "*Pulseaudio Sink Inputs*"))))
    (with-current-buffer buffer 
      (unless (equal major-mode 'pulseaudio-sink-inputs-mode)
        (pulseaudio-sink-inputs-mode))
      (pulseaudio-sink-inputs-refresh))
    (pop-to-buffer buffer)))

(provide 'pulseaudio-sink-inputs)
;;; pulseaudio-sink-inputs.el ends here
