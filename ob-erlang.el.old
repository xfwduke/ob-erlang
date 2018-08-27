;;; ob-erlang.el --- org-babel functions for erlang - lexical-binding: t -*-

;; Copyright (C) 2018  Free Software Foundation, Inc.
;; Author: xfwduke
;; Keywords: literate programing, reproducible research
;; Homepage: https://github.com/xfwduke/ob-erlang

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Org-mode language support for erlang
;; code block parameter description
;; :module
;;    specify erlang module, it will not cover -module line in the code
;; :start
;;    specify erlang entry function if code entry function is not start/0

;;; Code:
(require 'ob)

;; borrowed from ob-exlixir
(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("erlang" . "erlang"))

;; borrowed from ob-exlixir
(defun org-babel-execute:erlang (body params)
  "Execute BODY with PARAMS."
  (let* ((full-body (org-babel-expand-body:generic body params))
	 (temp-dir (make-temp-file "org-babel-erlang" t))
	 ;; erlang must defined a module and has same name with source code file
	 ;; so use module-name as source code filename
	 ;; if code has -module line, use it
	 ;; otherwise use parameter :module of code block
	 (module-name (if (string-match-p "^-module[[:blank:]]*([[:blank:]]*[[:alnum:]]+[[:blank:]]*)" full-body)
			  (progn
			    (string-match "^-module[[:blank:]]*([[:blank:]]*\\([[:alnum:]]+\\)[[:blank:]]*)" full-body)
			    (match-string 1 full-body))
			(progn
			  (defvar module-name (cdr (assoc ':module params)))
			  (setq full-body (concat "-module(" module-name ").\n" full-body))
			  module-name)))
	 ;; entry function must be specified to execute erlang code
	 ;; the default entry function is start
	 (start-fun (cdr (assoc ':start params)))
	 (src-file (expand-file-name  (concat module-name ".erl") temp-dir))
	 (results (progn (with-temp-file src-file (insert
						   (append-export-all full-body)))
	 		 (org-babel-eval
	 		  (concat "erlc -o " temp-dir " " src-file) "")
	 		 (org-babel-eval
	 		  (concat "erl -noshell -pa " temp-dir " -s " module-name " " start-fun " -s init stop") ""))))
    
    (delete-directory temp-dir t)
    
    (org-babel-reassemble-table
     results
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

(defun append-export-all (code-body)
  "Append -compile(export_all).  after -module line if CODE-BODY do not has export line."
  (if (export-p code-body)
      code-body
    (string-join (mapcar
		  (lambda (line)
		    (if (string-match-p "^-module" line)
			(setq line (concat line "\n-compile(export_all)."))
		      line))
		  (split-string code-body "\n"))
     "\n")))

(defun export-p (code-body)
  "Check CODE-BODY if has export line."
  (let ((has-export nil))
    (dolist (line (split-string code-body "\n") has-export)
      (if (string-match-p "^-export" line)
	  (return t)))))

;; borrowed from ob-exlixir
(defun org-babel-variable-assignments:erlang (params)
  (mapcar
   (lambda (pair)
     (format "%s = %s"
	     (car pair)
	     (org-babel-erlang-var-to-erlang (cdr pair))))
   (unless 'org-babel--get-vars
     (mapcar #'cdr (org-babel-get-header params :var))
     (org-babel--get-vars params))))

(provide 'ob-erlang)
;;; ob-erlang.el ends here

