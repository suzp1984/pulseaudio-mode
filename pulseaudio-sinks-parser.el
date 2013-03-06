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
                      :driver nil
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
  (setq state-data (plist-put state-data :driver nil))
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

(define-enter-state pa-sinks-parser :sinks-start
  (fsm state-data)
  (when (plist-get state-data :num)
    (message "add to state-data: %s" (plist-get state-data :num))
    (add-to-list 'pa-sinks-list (copy-sequence state-data))
    (setq state-data (plist-put state-data :num nil))
    (setq state-data (plist-put state-data :state nil))
    (setq state-data (plist-put state-data :name nil))
    (setq state-data (plist-put state-data :description nil))
    (setq state-data (plist-put state-data :driver nil))
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
    (setq state-data (plist-put state-data :formats nil)))
  (list state-data nil)
  )
(define-state pa-sinks-parser :sinks-start
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :num (cdr-safe event))
     (list :sinks-start state-data))))

(define-enter-state pa-sinks-parser :sinks-state
  (fsm state-data)
  (list state-data nil))
(define-state pa-sinks-parser :sinks-state
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :state (cdr-safe event))
     (list :sinks-state state-data))))

(define-state pa-sinks-parser :sinks-name
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :name (cdr-safe event))
     (list :sinks-name state-data))))

(define-state pa-sinks-parser :sinks-description
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value 
     (plist-put state-data :description (cdr-safe event))
     (list :sinks-description state-data))))

(define-state pa-sinks-parser :sinks-driver
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value 
     (plist-put state-data :driver (cdr-safe event))
     (list :sinks-driver state-data))))

(define-state pa-sinks-parser :sinks-sample
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :sample-specification (cdr-safe event))
     (list :sinks-sample state-data))))

(define-state pa-sinks-parser :sinks-channel
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :channel-map (cdr-safe event))
     (list :sinks-channel state-data))))

(define-state pa-sinks-parser :sinks-owner-module
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :owner-module (cdr-safe event))
     (list :sinks-owner-module state-data))))

(define-state pa-sinks-parser :sinks-mute 
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :mute (cdr-safe event))
     (list :sinks-mute state-data))))

(define-state pa-sinks-parser :sinks-volume
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :volume (cons (cdr-safe event) (plist-get state-data :volume)))
     (list :sinks-volume state-data))))

(define-state pa-sinks-parser :sinks-base-volume
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :base-volume (cons (cdr-safe event) (plist-get state-data :base-volume)))
     (list :sinks-base-volume state-data))))

(define-state pa-sinks-parser :sinks-monitor-source
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :monitor-source (cdr-safe event))
     (list :sinks-monitor-source state-data))))

(define-state pa-sinks-parser :sinks-latency 
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :latency (cdr-safe event))
     (list :sinks-latency state-data))))

(define-state pa-sinks-parser :sinks-flags
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :flags (cdr-safe event))
     (list :sinks-flags state-data))))

(define-state pa-sinks-parser :sinks-properties
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :properties (cons (cdr-safe event)
                                             (plist-get state-data :properties)))
     (list :sinks-properties state-data))))

(define-state pa-sinks-parser :sinks-ports
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :ports (cons (cdr-safe event)
                                        (plist-get state-data :ports)))
     (list :sinks-ports state-data))))

(define-state pa-sinks-parser :sinks-active-port
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :active-port 
                (cdr-safe event))
     (list :sinks-active-port state-data))))

(define-state pa-sinks-parser :sinks-formats
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (let ((value (cdr-safe event)))
       (when (string-match-p "[^ \t\n]" value) 
         (plist-put state-data :formats value))
       )
     (list :sinks-formats state-data))))

(provide 'pulseaudio-sinks-parser)
;;; pulseaudio-sinks-parser.el ends here
