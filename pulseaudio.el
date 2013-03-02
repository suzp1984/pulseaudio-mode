;;; pulseaudio.el --- pulseaudio control panel

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

(defgroup pulseaudio nil
  "A PulseAudio Management in Emacs"
  :tag "The PulseAudio Management"
  :version 0.1
  :group 'applications)

(define-derived-mode pulseaudio-mode fundamental-mode "PulseAudio Manager"
  "Major mode for PulseAudio Management."
  )

(defun pulseaudio ()
  "Inter pulseaudio management mode."
  (interactive)
  )

(defun pulseaudio-quit-windows (&optional kill-buffer)
  "Kill the buffer and quit the windows"
  (interactive "P")
  (quit-window kill-buffer (selected-window)))

(provide 'pulseaudio)
;;; pulseaudio.el ends here
