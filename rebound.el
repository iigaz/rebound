;;; rebound.el --- Safely rebind the prefix keys. -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ilnar Gazizov

;; Author: Ilnar Gazizov
;; Created: 01 Mar 2026
;; Keywords: convenience
;; URL: https://github.com/iigaz/rebound

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The intended purpose of the package is to give you the ability to
;; configure `rebound-cc-key' and `rebound-cx-key' to be used in place
;; of C-c and C-x respectively, so that you can safely bind functions
;; to C-c and C-x -- preferably in `rebound-key-map' or
;; `rebound-important-key-map', which are only activated when the mode
;; is active.  The latter map allows you to override minor and
;; major mode key maps (but still can be overwritten by some maps in
;; `emulation-mode-map-alists' as well as `overriding-local-map' and
;; `overriding-terminal-local-map').

;; Most of the code was adapted from the awesome package called
;; wakib-keys by Abdulla Bubshait, also licensed GPLv3-or-later
;; (URL: https://github.com/darkstego/wakib-keys/ ).  In contrast to
;; the said package, the goal of rebound is not to provide modern
;; keybindings, but to aid in customizing default Emacs ones.  So only
;; parts of the code responsible for moving C-c and C-x to C-d and C-e
;; were taken from wakib-keys.  If you want more opinionated,
;; ready-to-use modern keybindings in Emacs, I highly recommend you to
;; check out wakib-keys instead.

;;; Code:

(defcustom rebound-cc-key "C-d"
  "Key to use as a prefix instead of C-c.
Changes take effect when `rebound-mode' is activated."
  :type 'string
  :group 'rebound-mode)

(defcustom rebound-cx-key "C-e"
  "Key to use as a prefix instead of C-x.
Changes take effect when `rebound-mode' is activated."
  :type 'string
  :group 'rebound-mode)

(defvar rebound-key-map (make-sparse-keymap)
  "Regular `rebound-mode' minor mode key map.")

(defvar rebound-important-key-map (make-sparse-keymap)
  "Key map that overrides regular key bindings.
It goes into `emulation-mode-map-alists', so it overrides minor modes,
local and global maps, but still can be overwritten by some maps,
e.g. `overriding-local-map'.")

(defun rebound--minor-mode-key-binding (key)
  "Return all keymaps defined to KEY within minor modes.

Copied with minor changes from wakib-keys."
  (let ((active-maps nil))
    (mapc (lambda (x)
	        (when (and (symbolp (car x)) (symbol-value (car x)))
	          (add-to-list 'active-maps
                           (lookup-key (cdr x) key))))
	      minor-mode-map-alist)
    (make-composed-keymap active-maps)))

(defun rebound--key-binding (key)
  "Return the full keymap bindings of KEY.

Copied with minor changes from wakib-keys."
  (make-composed-keymap
   (list
    (rebound--minor-mode-key-binding key)
    (local-key-binding key)
    (global-key-binding key))))

(defun rebound-dynamic-binding (key)
  "Act as KEY in the current context.
This uses an extended menu item's capability of dynamically computing
a definition.

Copied with minor changes from wakib-keys,
the original idea was taken from general.el."
  `(menu-item ,"" nil
              :filter ,(lambda (&optional _)
                         (rebound--key-binding key))))

(defun rebound--function-lookup (fun)
  "Lookup FUN in C-c/C-x maps and return the updated description.
If FUN is not in C-c/C-x maps, return nil.

Copied with minor changes from wakib-keys."
  (let ((cx-key
         (where-is-internal fun
                            (list (rebound--key-binding [?\C-x]))
                            'non-ascii))
	    (cc-key
         (where-is-internal fun
                            (list (rebound--key-binding [?\C-c]))
                            'non-ascii)))
    (cond
     (cc-key (concat rebound-cc-key " " (key-description cc-key)))
	 (cx-key (concat rebound-cx-key " " (key-description cx-key)))
	 (t nil))))

(defun rebound--substitute-command-keys-advice (orig-fun &rest args)
  "Advice for `substitute-command-keys' to substitute prefix keys.

The idea was taken from wakib-keys, but this version does not use
hash tables because I did not found them useful (they get recomputed
every call anyway, and there were no duplicate matches for a single
call in my tests)."
  (if (stringp (car args))
      (let ((str (replace-regexp-in-string
                  "\\\\\\[\\([^\]]*\\)\\]" ; matches \[COMMAND]
                  (lambda (match)
                    (let ((key (rebound--function-lookup
                                (intern (substring match 2 -1)))))
                      (if key key match)))
				  (car args) t t)))
        (apply orig-fun (list str)))
    (apply orig-fun args)))

(defun rebound--replace-in-region (regex rep start-point end-point)
  "Replace REGEX with REP from START-POINT to END-POINT in buffer.

Copied from wakib-keys."
  (save-excursion
    (goto-char start-point)
    (while (re-search-forward regex end-point t)
      (replace-match rep))))

(defun rebound--describe-buffer-bindings-advice (orig-fun buffer
  &optional prefix menus)
  "Advice for `describe-buffer-bindings' to show substituted bindings.

The idea was taken from wakib-keys. Changes include fixed
descriptions for C-x."
  (let ((start-point (point))
        (string-prefix (key-description prefix))
        (cc-sp (concat rebound-cc-key " "))
        (cx-sp (concat rebound-cx-key " "))
        (cc-hat (concat "^" rebound-cc-key))
        (cx-hat (concat "^" rebound-cx-key)))
    (cond
     ((not prefix)
	  (apply orig-fun buffer prefix menus)
	  (rebound--replace-in-region "^C-c " cc-sp start-point (point))
	  (rebound--replace-in-region "^C-x " cx-sp start-point (point)))
	 ((string-match-p cc-hat string-prefix)
	  (apply orig-fun buffer
	         (kbd (replace-regexp-in-string cc-hat "C-c"
                                            string-prefix))
             menus)
	  (rebound--replace-in-region "^C-c " cc-sp start-point (point)))
	 ((string-match-p cx-hat string-prefix)
	  (apply orig-fun buffer
	         (kbd (replace-regexp-in-string cx-hat "C-x"
                                            string-prefix))
             menus)
	  (rebound--replace-in-region "^C-x " cx-sp start-point (point)))
	 (t (apply orig-fun buffer prefix menus)))))

(defvar rebound-mode--prefix-keys-map (make-sparse-keymap)
  "This map is only intended for C-c/C-x prefix keys.
Expect it to get cleared by `rebound--update-keys'.")

(defun rebound--update-keys ()
  "Clear previous C-c/C-x bindings and rebind them to new values."
  (setq rebound-mode--prefix-keys-map (make-sparse-keymap))
  (define-key rebound-mode--prefix-keys-map (kbd rebound-cx-key)
              (rebound-dynamic-binding (kbd "C-x")))
  (define-key rebound-mode--prefix-keys-map (kbd rebound-cc-key)
              (rebound-dynamic-binding (kbd "C-c")))
  (setq emulation-mode-map-alists
        (seq-remove (lambda (x)
                      (when (and (listp x) (listp (car x)))
                        (eq (caar x) 'rebound-mode)))
                    emulation-mode-map-alists))
  (add-to-list 'emulation-mode-map-alists
	           `((rebound-mode . ,(append
                                   rebound-mode--prefix-keys-map
                                   (cdr
                                    rebound-important-key-map))))))

;;;###autoload
(define-minor-mode rebound-mode
  "A global minor mode which allows custom keys to act as C-x or C-c.
This allows you to bind functions on C-x and C-c without missing on
prefixed bindings."
  :lighter " Rebnd"
  :init-value nil
  :require 'rebound-mode
  :global t
  :keymap rebound-key-map
  :group 'rebound-mode
  (rebound--setup))

(defun rebound--setup ()
  "Setup the `rebound-mode' minor mode."
  (if rebound-mode
      (progn
        (advice-add 'substitute-command-keys
                    :around #'rebound--substitute-command-keys-advice)
	    (advice-add 'describe-buffer-bindings
                    :around
                    #'rebound--describe-buffer-bindings-advice)
        (rebound--update-keys))
    (advice-remove 'substitute-command-keys
                   #'rebound--substitute-command-keys-advice)
    (advice-remove 'describe-buffer-bindings
                   #'rebound--describe-buffer-bindings-advice)))

(provide 'rebound-mode)

;;; rebound.el ends here
