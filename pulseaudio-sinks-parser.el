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

(define-enter-state pa-sinks-parser :null-state)

(provide 'pulseaudio-sinks-parser)
;;; pulseaudio-sinks-parser.el ends here
