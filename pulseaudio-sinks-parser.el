;;; pulseaudio-sinks-parser.el --- State machine defination for pusleaudio output

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

(require 'fsm)

(define-state-machine pa-sinks-parser
  :start ((output)
          "start routine to parser pulseaudio output string."
          (list :null-state
                (list :lines (split-string output "\n" t)
                      :num nil
                      :state nil
                      :name nil
                      :description nil
                      :sample-specification nil
                      :channel-map nil
                      :owner-module nil
                      :mute nil
                      :volume nil
                      :base-volume nil
                      :monitor-source nil
                      :latency nil
                      :flags nil
                      :properties nil
                      :ports nil
                      :active-port nil
                      :formats nil
                      ))))

(define-enter-state pa-sinks-parser :null-state
  (fsm state-data)
  (setq state-data (plist-put state-data :num nil))
  (setq state-data (plist-put state-data :state nil))
  (setq state-data (plist-put state-data :name nil))
  (setq state-data (plist-put state-data :description nil))
  (setq state-data (plist-put state-data :sample-specification nil))
  (setq state-data (plist-put state-data :channel-map nil))
  (setq state-data (plist-put state-data :owner-module nil))
  (setq state-data (plist-put state-data :mute nil))
  (setq state-data (plist-put state-data :volume nil))
  (setq state-data (plist-put state-data :base-volume nil))
  (setq state-data (plist-put state-data :monitor-source nil))
  (setq state-data (plist-put state-data :latency nil))
  (setq state-data (plist-put state-data :flags nil))
  (setq state-data (plist-put state-data :properties nil))
  (setq state-data (plist-put state-data :ports nil))
  (setq state-data (plist-put state-data :active-port nil))
  (setq state-data (plist-put state-data :formats nil))
  (setq pa-sinks-list nil)
  (list state-data nil))
(define-state pa-sinks-parser :null-state
  (fsm state-data event callback)
  (list :null-state state-data))

(provide 'pulseaudio-sinks-parser)
;;; pulseaudio-sinks-parser.el ends here
