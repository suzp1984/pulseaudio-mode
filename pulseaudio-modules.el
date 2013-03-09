;;; pulseaudio-modules.el --- pulseaudio modules mode 

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: zpcat su <suzp1984@gmail.com>
;; Version: 0.0
;; Package-Requires: (())
;; Keywords: pulseaudio-modules

;;; Commentary:

;; This package define a major mode of pulseaudio moudules manager.
;;

(require 'tabulated-list)
(require 'cl)
;;(require 'pulseaudio-modules-parse)

(defgroup pulseaudio-modules nil
  "PulseAudio Modules"
  :tag "The PulseAudio Modules Management"
  :version "0.1"
  :group 'pulseaudio)

(defcustom pa-modules-location-paths '("/usr/lib64/pulse-2.1/modules/")
  "A list of locations to search."
  :type '(repeat string)
  :group 'pulseaudio-modules)

(defcustom pa-modules-buffer-name "*PulseAudio Modules*"
  "PulseAudio Modules mode's buffer name."
  :type 'string
  :group 'pulseaudio-modules)

(defface pa-module-loaded-face 
  '((t (:foreground "GREEN" :weight bold)))
  "Loaded module Face")

(defface pa-module-unloaded-face 
  nil
  "Unloaded module Face")

(defvar pa-modules-all-list nil
  "All pulseaudio modules modules,
In format of (num name argument proerties other)")

(defvar pa-modules-loaded-list nil
  "All loaded pulseaudio modules")

(defvar pa-modules-parser nil)

(defvar pulseaudio-modules-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'pulseaudio-load-module)
    (define-key map (kbd "L") 'pulseaudio-load-module)
    (define-key map (kbd "U") 'pulseaudio-unload-module)
    map)
  "pulseaudio-modules-mode keymap")

;;;###autoload
(define-derived-mode pulseaudio-modules-mode tabulated-list-mode "PulseAudio Modules"
  "Major mode for PulseAudio Modules Management."
  (setq tabulated-list-format [("Num" 3 t)
                               ("Name" 25 t)
                               ("Argument" 20 nil)
                               ("Properties" 0 nil)])
  ;;(setq tabulated-list-sort-key (cons "Num" nil))
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook 'pa-modules-refresh nil t)
  (tabulated-list-init-header))

(defun pulseaudio-load-module (name &optional args)
  "Load current module described in current point."
  (interactive (let ((module (tabulated-list-get-id))
                     name
                     args)
                 (setq name (completing-read
                             (concat "Default to load module [" module "]: ")
                             tabulated-list-entries nil t nil nil module))
                 (setq args (read-string
                             (concat "Input default load argument: ")))
                 (list name args)))
  (message "%s" (concat "pactl  load-module " "module-" name " " args))
  (when (y-or-n-p (concat "Are you sure to load module (" name ")" 
                          (if args 
                              (concat " with arguments - " args)
                            " without arguments.")
                          )) 
    (call-process "/usr/bin/pactl" nil nil nil "load-module" (concat "module-" name) args)
    (pa-modules-refresh)
    (tabulated-list-print))
  )

(defun pulseaudio-unload-module (name)
  "Unload current module at current point"
  (interactive (let ((module (tabulated-list-get-id))
                     result)
                 (setq result (completing-read
                               (concat "Default to unload module [" module "]: ")
                               tabulated-list-entries nil t nil nil module))
                 (list result)))
  (when (y-or-n-p (concat "Are you sure to unload module (" name ") "))
    (let ((list-entries tabulated-list-entries)
          id
          )
      (pa-modules-refresh)
      (while (car-safe list-entries)
        (setq entry (car list-entries))
        (setq list-entries (cdr-safe list-entries))
        (when (string-equal name (car-safe entry))
          (setq list-entries nil)
          (setq id (elt (nth 1 entry) 0))
          (message "id: %s" id)
          (call-process "/usr/bin/pactl" nil nil nil "unload-module" id)
          )
        )
      )
    ;;(sit-for 2)
    (pa-modules-refresh)
    (tabulated-list-print)))

(defun pa-modules-refresh ()
  "fill pa-modules-all-list"
  (setq tabulated-list-entries nil)
  (let ((output (shell-command-to-string "pactl list modules")))
    (setq pa-modules-parser (start-pa-modules-parser output))
    (dolist (line (split-string output "\n" t))
      (cond ((string-match "^Module #\\([0-9]+\\)" line)
             (let* ((num (string-to-int (match-string 1 line)))
                    (event (cons :value (int-to-string num))))
               (fsm-update pa-modules-parser :module-start 
                           (fsm-get-state-data pa-modules-parser) nil)
               (fsm-send-sync pa-modules-parser event)))
            ((string-match "^[ \t]*Name:[ \t]*\\(.*\\)" line)
             (let* ((name (replace-regexp-in-string "^module-" "" (match-string 1 line)))
                    (event (cons :value name)))
               (fsm-update pa-modules-parser :module-name
                           (fsm-get-state-data pa-modules-parser) nil)
               (fsm-send-sync pa-modules-parser event)
               )
             )
            ((string-match "^[ \t]*Argument:[ \t]*\\(.*\\)" line)
             (let* ((argument (match-string 1 line))
                    (event (cons :value argument)))
               (fsm-update pa-modules-parser :module-argument
                           (fsm-get-state-data pa-modules-parser) nil)
               (fsm-send-sync pa-modules-parser event))
             )
            ((string-match "^[ \t]*Properties:[ \t]*\\(.*\\)" line)
             (fsm-update pa-modules-parser :module-properties
                         (fsm-get-state-data pa-modules-parser) nil)
             )
            (t 
             (let* ((proper (trim-string line))
                    (event (cons :proper proper))) 
               (fsm-send-sync pa-modules-parser event))
             )
            )
      )
    (fsm-update pa-modules-parser :module-start
                (fsm-get-state-data pa-modules-parser) nil))
  (dolist (path pa-modules-location-paths)
    ;;(message "%s" path)
    (dolist (file (directory-files path nil "^module-.*.so$" nil))
      ;; add to list if module is not loaded
      (let ((name (replace-regexp-in-string 
                   "^module-" "" 
                   (replace-regexp-in-string ".so$" "" file)))
            (loaded nil))
        (dolist (p pa-modules-all-list)
          (if (string-equal name (plist-get p :name))
              (setq loaded t)))
        (unless loaded
          (add-to-list 'pa-modules-all-list 
                       (list :num "" :name name
                             :argument ""
                             :properties ""))))
      )
    )
  (dolist (p pa-modules-all-list)
    (push (list (plist-get p :name) 
                (vector 
                 (plist-get p :num)
                 (plist-get p :name)
                 (plist-get p :argument)
                 (plist-get p :properties)))
          tabulated-list-entries)))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )

;;;###autoload
(defun list-pulseaudio-modules ()
  "Display a list of pulseaudio modules"
  (interactive)
  (let ((buffer (get-buffer-create 
                 (or pa-modules-buffer-name "*PulseAudio Modules*")))
        ) 
    (with-current-buffer buffer
      (pulseaudio-modules-mode)
      (pa-modules-refresh)
      (tabulated-list-print))
    (pop-to-buffer buffer))
  )

(provide 'pulseaudio-modules)
