;;; acm-history.el --- Sorting by history for Acm -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (acm "0.26"))
;; Homepage: https://github.com/minad/acm

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Enable `acm-history-mode' to sort candidates by their history
;; position. Maintain a list of recently selected candidates. In order
;; to save the history across Emacs sessions, enable `savehist-mode' and
;; add `acm-history' to `savehist-additional-variables'.
;;
;; (acm-history-mode 1)
;; (savehist-mode 1)
;; (add-to-list 'savehist-additional-variables 'acm-history)

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defcustom acm-history-length 300
  "Acm history length."
  :type '(choice (const nil) integer))

(defvar acm-history--hash nil
  "Hash table of Acm candidates.")

(defvar acm-history nil
  "History of Acm candidates.")

(defun acm-history--sort-predicate (x y)
  "Sorting predicate which compares X and Y."
  (let ((sx (plist-get x :label))
	(sy (plist-get y :label))
	(hx (plist-get x :order-idx))
	(hy (plist-get y :order-idx)))
    (or (< hx hy)
      (and (= hx hy)
           (or (< (length sx) (length sy))
               (and (= (length sx) (length sy))
                    (string< sx sy)))))))

(defun acm-history--sort (keyword candidates)
  "Sort CANDIDATES by history."
  (setq acm-history--hash (make-hash-table :test #'equal :size (length acm-history)))
  (cl-loop for elem in acm-history for index from 0 do
           (unless (gethash elem acm-history--hash)
             (puthash elem index acm-history--hash)))
  ;; Decorate each candidate with (index<<13) + length. This way we sort first by index and then by
  ;; length. We assume that the candidates are shorter than 2**13 characters and that the history is
  ;; shorter than 2**16 entries.
  (dolist (cand candidates)
	   (plist-put cand :order-idx
                              (+ (ash (gethash (plist-get cand :label) acm-history--hash #xFFFF) 13)
                                 (length (plist-get cand :label)))))
  (setq candidates (sort candidates #'acm-history--sort-predicate))
  candidates)

(defun acm-history--insert (&rest _)
  "Advice for `acm--insert'."
  (when (>= acm-menu-index 0)
    (add-to-history 'acm-history
                    (substring-no-properties
		     (plist-get
                      (acm-menu-current-candidate) :label))
                    acm-history-length)
    (setq acm-history--hash nil)))

;;;###autoload
(define-minor-mode acm-history-mode
  "Update Acm history and sort completions by history."
  :global t
  (cond
   (acm-history-mode
    (setq acm-sort-function #'acm-history--sort)
    (advice-add #'acm-complete :before #'acm-history--insert))
   (t
    (setq acm-sort-function #'acm-candidate-sort-by-prefix)
    (advice-remove #'acm-complete #'acm-history--insert))))

(provide 'acm-history)
;;; acm-history.el ends here
