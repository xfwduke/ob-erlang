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
	 ;; erlang must define a module whose name matches the source
	 ;; code file name.  The module name from which the source
	 ;; code file name is derived is taken from the sources when a
	 ;; -module line is present, else it is taken from the :module
	 ;; parameter of the code block, if present.  If none are
	 ;; defined, the module name "m" is used, because a nil module
	 ;; name is invalid in Erlang.
	 (module-name (if (string-match-p "^-module[[:blank:]]*([[:blank:]]*[[:alnum:]]+[[:blank:]]*)" full-body)
			  (progn
			    (string-match "^-module[[:blank:]]*([[:blank:]]*\\([[:alnum:]]+\\)[[:blank:]]*)" full-body)
			    (match-string 1 full-body))
			(let ((m (or (cdr (assoc ':module params)) "m")))
			  (setq full-body (concat "-module(" m ").\n" full-body))
			  m)))
	 ;; entry function must be specified to execute erlang code
	 ;; the default entry function is start
	 (start-fun (cdr (assoc ':start params)))
	 (cookie (cdr (assoc ':cookie params)))
	 (name (cdr (assoc ':name params)))
	 (src-file (expand-file-name  (concat module-name ".erl") temp-dir))
	 (results (progn (with-temp-file src-file (insert
						   (append-export-all full-body)))
	 		 (org-babel-eval
	 		  (concat "erlc -o " temp-dir " " src-file) "")
	 		 (org-babel-eval
	 		  (concat "erl -noshell " " -name " name " -setcookie " cookie " -pa " temp-dir " -s " module-name " " start-fun " -s init stop") ""))))
    
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

