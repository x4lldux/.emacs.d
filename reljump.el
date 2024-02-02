;;; reljump.el --- Relative line jumping

;; Author: X4lldux <x4lldux@vectron.io>
;; Created: Mon Jul  3 20:38:15 2023
;; Keywords: relative, navigation, jumping
;; Package-Requires: ((dash "2.11.0") (linum-relative "0.6"))
;; Version: 0.1

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
;; This package provides functionality of relative jumping using home keys row.
;; It's based on `vertigo' package, but adds support for modifiier keys and
;; displaying relative line numbers.

;;; Original comment
;; This package is a port of the vim-vertigo plugin and gives commands that
;; allow the user to jump up or down a number of lines using the home row.
;; It will primarily be useful when relative line numbers are being used. To
;; jump down seven lines, for example, the user can press a key bound to
;; `vertigo-jump-down' and then press "j" since it is the seventh letter on the
;; home row. `vertigo-home-row' can be altered for non-QWERTY users. Since it is
;; unlikely that the user will want to use these commands to jump down one or
;; two lines, `vertigo-cut-off' can be set to determine that the first n keys
;; should accept another key afterwards. For example, if `vertigo-cut-off' is
;; set to its default value of 3, pressing "da" would jump 31 lines, pressing
;; "d;" would jump 30 lines, and pressing "f" would jump 4 lines.

;; A good alternative to this package is to use avy's `avy-goto-line'.

;; Additionally, vertigo provides commands to set the digit argument using the
;; same style of keypresses.

;; For more information see the README in the github repo.

;;; Code:
(require 'dash)
(require 'cl-lib)

(defgroup reljump nil
  "Gives commands for moving by lines using the home row."
  :group 'convenience
  :prefix "reljump-")

(defcustom reljump-home-row
  '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)
  "10 chars corresponding to the home row keys (or the numbers 1-9 and 0)."
  :group 'reljump
  :type '(repeat char))

(defcustom reljump-jump-up-modifiers
  '(meta)
  "10 chars corresponding to the home row keys (or the numbers 1-9 and 0)."
  :group 'reljump
  :type '(repeat (choice (const 'meta)
                         (const 'shift)
                         (const 'control)
                         (const 'super)
                         (const 'hyper))))

(defcustom reljump-cut-off 3
  "This determines boundary key for whether one or two keys should be input.
For example, with the default value of 3, 39 lines at max can be jumped. The
third key in `reljump-home-row' will jump 30 something lines (depending on the
second keypress). On the other hand, pressing the fourth key will jump down 4
lines without further user input. Setting this value to 0 will make all keys
immediately jump. Setting it to 10 will make no keys immediately jump. Note that
this variable only has an effect when `reljump-max-digits' is 2. Regardless of
the value of these variables, inputting an uppercase letter will immediately
end the number (e.g. by default, inputting \"A\" corresponds to 1)."
  :type 'integer)

(defcustom reljump-max-digits 2
  "The max number of digits that can be specified with reljump commands.
The default value is 2, meaning that the max number that can be specified is
99."
  :type 'integer)

(defvar reljump--display-line-numbers-enabled nil
  "Whether `display-line-numbers-mode' was enabled.")

(defun reljump--relative-symbols-from-home-row ()
  (-concat
   ;; cut-off lines, no jump
   (cl-loop
    for x to reljump-cut-off
    collect "  ")

   ;; immediate jump single-letter lines, except symbols before cut-off
   (cl-loop
    for x in (-drop reljump-cut-off reljump-home-row)
    collect (byte-to-string x))

   ;; composed letters jump lines, starting only with symbols before cut-off
   (cl-loop
    for x in (-take reljump-cut-off reljump-home-row)
    nconc (cl-loop
           for y in reljump-home-row
           collect (concat (byte-to-string x) (byte-to-string y))))
   ))

(defvar reljump--relative-symbols (reljump--relative-symbols-from-home-row)
  "List of symbols to be displayed as line numbers")

(defun reljump-linum-relative-format (line-number)
  (when (linum-relative-in-helm-p)
    (linum-relative-with-helm-buffer
     (if (looking-at helm-candidate-separator)
         (setq line-number (save-excursion
                             (forward-line 1) (helm-candidate-number-at-point)))
       (setq line-number (helm-candidate-number-at-point)))))
  (let* ((diff1 (abs (- line-number linum-relative-last-pos)))
         (diff (if (minusp diff1)
                   diff1
                 (+ diff1 linum-relative-plusp-offset)))
         (current-p (= diff linum-relative-plusp-offset))
         (current-symbol-tmp (if (and linum-relative-current-symbol current-p)
                                 (if (string= "" linum-relative-current-symbol)
                                     (number-to-string line-number)
                                   linum-relative-current-symbol)
                               (or (nth diff reljump--relative-symbols)
                                   " ")))
         (current-symbol (format "%5s " ; 1 space for boarder, 4 spaces padding
                                 (if (minusp (- line-number linum-relative-last-pos))
                                     (upcase current-symbol-tmp)
                                   current-symbol-tmp)
                                 ))
         (face (if current-p 'linum-relative-current-face 'linum)))
    (if (and (linum-relative-in-helm-p)
             (linum-relative-with-helm-buffer
              (or (looking-at helm-candidate-separator)
                  (eq (point-at-bol) (point-at-eol))
                  (helm-pos-header-line-p))))
        (propertize (format linum-relative-format current-symbol) 'invisible t)
      (propertize (format linum-relative-format current-symbol) 'face face))))


(cl-defun reljump--get-num (prompt &optional no-message)
  "Call FUNCTION with a count determined by user-input characters.
PROMPT is the prompt to display when asking users to input keys
and calling FUNCTION. When NO-MESSAGE is non-nil, don't message
with the prompt and chosen number afterward calling FUNCTION."
  (let ((num-digits 1)
        (immediate-end-chars (-drop reljump-cut-off reljump-home-row))
        (jump-direction 1)
        current-str)
    (while (let* ((key (read-char (concat prompt current-str)))
                  (char (downcase (event-basic-type key)))
                  (index (or (-elem-index char reljump-home-row)
                             (cl-return-from reljump--get-num 0)))
                  (endp (or (= num-digits reljump-max-digits)
                            (and (= reljump-max-digits 2)
                                 (memq char immediate-end-chars)))))
             (when (equal (event-modifiers key) reljump-jump-up-modifiers)
               (setq jump-direction -1))
             (cl-incf num-digits)
             (setq current-str (concat current-str (byte-to-string char)))
             (not endp)))
    (let ((final-num (* jump-direction (-elem-index current-str reljump--relative-symbols))))
      (unless no-message
        (message (concat prompt current-str " --")))
      final-num)))

(defun reljump--linum-on ()
  (if (memq 'display-line-numbers-mode minor-mode-list)
      (progn
        (setq reljump--display-line-numbers-enabled 't)
        (display-line-numbers-mode -1))
    (setq reljump--display-line-numbers-enabled nil))

  (unless (eq linum-format 'reljump-linum-relative-format)
    (setq linum-relative-user-format linum-format)
    (setq linum-format 'reljump-linum-relative-format))
  (linum-mode 1))

(defun reljump--linum-off ()
  (setq linum-format linum-relative-user-format)
  (linum-mode -1)
  (when reljump--display-line-numbers-enabled
    (display-line-numbers-mode 1)))

(defun reljump-run (&optional pre-jump-function post-jump-function)
  "Jump up or down depending on what keys you press.
  PRE-JUMP-FUNCTION and POST-JUMP-FUNCTION will be called before
  and after jumping, respectivly."

  (unwind-protect
      (progn
        (reljump--linum-on)
        (when pre-jump-function
          (funcall pre-jump-function))
        (forward-line (reljump--get-num "Jump: ")))
    (progn
      (reljump--linum-off)
      (when post-jump-function
        (funcall post-jump-function)))))

(defun reljump ()
  "Jump up or down depending on what keys you press."
  (interactive)
  (reljump-run))

(provide 'reljump)
;;; reljump.el ends here
