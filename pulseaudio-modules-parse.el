;;; pulseaudio-modules-parse.el --- define status machine for pulseaudio modules

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
(require 'pulseaudio-modules)

(define-state-machine pa-modules-parser
  :start ((output)
          "Start Parse pulseaudio output parser."
          (list :null-state
                (list :lines (split-string output "\n" t)
                      :num nil
                      :name nil
                      :argument nil
                      :properties nil)))
  )

(define-enter-state pa-modules-parser :null-state
  (fsm state-data)
  (setq state-data (plist-put state-data :num nil))
  (setq state-data (plist-put state-data :name nil))
  (setq state-data (plist-put state-data :argument nil))
  (setq state-data (plist-put state-data :properties nil))
  (setq pa-modules-all-list nil)
  ;; call parse function
  (list state-data nil))
(define-state pa-modules-parser :null-state
  (fsm state-data event callback)
  (list :null-state state-data)
  )

(define-enter-state pa-modules-parser :module-start
  (fsm state-data)
  ;; if num not null, do something
  (when (plist-get state-data :num)
    (let ((num (plist-get state-data :num))
          (name (propertize (plist-get state-data :name) 'face 'pa-module-loaded-face))
          (argument (plist-get state-data :argument))
          (properties (plist-get state-data :properties)))
      (add-to-list 'pa-modules-all-list 
                   (list :num num 
                         :name name 
                         :argument argument 
                         :properties properties)))
    (plist-put state-data :num nil)
    (plist-put state-data :name nil)
    (plist-put state-data :argument nil)
    (plist-put state-data :properties nil))
  (list state-data nil))
(define-state pa-modules-parser :module-start
  (fsm state-data event callback)
  (case (or (car-safe event) event) 
    (:value
     (plist-put state-data :num (cdr-safe event))
     (list :module-start  state-data)))
  )

(define-enter-state pa-modules-parser :module-name
  (fsm state-data)
  (list state-data nil))
(define-state pa-modules-parser :module-name
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value 
     (plist-put state-data :name (cdr-safe event))
     (list :module-name state-data)))
)

(define-enter-state pa-modules-parser :module-argument
  (fsm state-data)
  (list state-data nil)
  )
(define-state pa-modules-parser :module-argument
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:value
     (plist-put state-data :argument (cdr-safe event))
     (list :module-argument state-data)))
  )

(define-enter-state pa-modules-parser :module-properties
  (fsm state-data)
  (list state-data nil)
  )
(define-state pa-modules-parser :module-properties
  (fsm state-data event callback)
  (case (or (car-safe event) event)
    (:proper
     (plist-put state-data :properties 
                (concat (plist-get state-data :properties)
                        ";" 
                        (cdr-safe event)))
     (list :module-properties state-data)))
  )

(provide 'pulseaudio-modules-parse)
;;; pulseaudio-modules-parse.el ends here
