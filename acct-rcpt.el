;;; acct-rcpt.el --- accounting receipts helper      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  David Asabina

;; Author: David Asabina <david@asabina.de>
;; Keywords: lisp
;; Version: 0.0.1

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

;; Helpers to aid in handling of monthly accounting duties

;;; Code:

(defgroup acct-rcpt nil
  "Accounting receipt processing helpers"
  :prefix "acct-rcpt-"
  :version "29.0.50")

(defcustom acct-rcpt-contexts '()
  "Accounting contexts"
  :type 'sexp
  :group 'acct-rcpt)

(defun acct-rcpt--valid-context-label (label)
  (cond ((not (stringp label))
         (cons nil (format "Label %S is not a string" label)))
        ((= (length (string-clean-whitespace label)) 0)
         (cons nil (format "Stripped label %S is not of nonzero length" label)))
        (t
         (cons (string-clean-whitespace label) nil))))

(defun acct-rcpt--valid-context-config (data)
  (cond ((not (and (listp data) (cl-every 'consp data)))
         (cons nil (format "Context data %S is not a valid alist" data)))
        ((not (stringp (alist-get :base-directory data)))
         (cons nil (format "Context data %S lacks :base-directory key with a string value" data)))
        (t
         (cons data nil))))

(defun acct-rcpt--valid-context (context)
  (cond ((not (consp context))
         (cons nil (format "Context %S not a cons cell" context)))
        ((let ((label (acct-rcpt--valid-context-label (car context)))
               (data (acct-rcpt--valid-context-config (cdr context))))
           (cons (when (and (car label) (car data)) context)
                 (seq-filter (lambda (x)
                               (not (eq nil x)))
                             (list (cdr label) (cdr data))))))
        (t (cons context nil))))

(defun acct-rcpt--valid-contexts (contexts)
  (cond ((not (and (listp contexts) (cl-every 'consp contexts)))
         (cons nil (format "Contexts %S not a valid alist" contexts)))
        ((= 0 (length contexts))
         (cons nil (format "Contexts %S of zero length" contexts)))
        ((let* ((cs (cl-map 'list 'acct-rcpt--valid-context contexts))
                (keys (cl-map 'list 'car cs))
                (vals (cl-map 'list 'cdr cs)))
           (when (seq-contains-p keys nil)
             (cons nil vals))))
        (t (cons contexts nil))))

(defun acct-rcpt-table-template (table-name)
  "Return accounting table template"
  (string-join
           (list (format "#+NAME: %s" table-name) "
|   |   Balance |  Diff | Date             | File              | Description      |
| ! |   Balance |  Diff | Date             | File              | Description      |
| / |           |       |                  | <6>               |                  |
|   | 100000.00 |     0 | [2022-01-01 Sat] |                   | starting balance |
|---+-----------+-------+------------------+-------------------+------------------|
| # | 103500.00 |  3500 |                  | [[file:acct-rcpt.el]] |                  |
| # |  98500.00 | -5000 |                  |                   |                  |
|---+-----------+-------+------------------+-------------------+------------------|
| # |  98500.00 |     0 | [2022-01-31 Mon] |                   | ending balance   |
#+TBLFM: $2=@-1+$+1;%.2f")))

(defun acct-rcpt-insert-table-template (table-name)
  "Insert accounting table template"
  (interactive "sTable name: ")
  (insert (acct-rcpt-table-template (table-name))))

(defun acct-rcpt-read-table (data)
  "Read entries from accounting table"
  (car data))

(provide 'acct-rcpt)
;;; acct-rcpt.el ends here
