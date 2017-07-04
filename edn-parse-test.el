;;; clj-parse-test.el --- Clojure/EDN parser

;; Copyright (C) 2017  Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the Mozilla Public License Version 2.0

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the Mozilla Public License along with this
;; program. If not, see <https://www.mozilla.org/media/MPL/2.0/index.txt>.

;;; Commentary:

;; A reader for EDN data files and parser for Clojure source files.

;;; Code:

(require 'edn-parse)
(require 'ert)

(defmacro edn-parse-deftest (name parse-to-fn test-string expected)
  (declare (indent defun))
  `(ert-deftest ,name ()
     (with-temp-buffer
       (insert ,test-string)
       (goto-char 1)
       (should (equal (,parse-to-fn) ,expected)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Elisp code

(edn-parse-deftest elisp-simple-list
  edn-parse-to-elisp
  "(1 2 3)"
  '((1 2 3)))

(edn-parse-deftest elisp-empty-list
  edn-parse-to-elisp
  "()"
  '(()))

(edn-parse-deftest elisp-list-size-1
  edn-parse-to-elisp
  "(1)"
  '((1)))

(edn-parse-deftest elisp-leafs
  edn-parse-to-elisp
  "(nil true false hello-world)"
  '((nil t nil hello-world)))

(edn-parse-deftest elisp-qualified-symbol
  edn-parse-to-elisp
  "clojure.string/join"
  '(clojure.string/join))

(edn-parse-deftest elisp-nested-lists
  edn-parse-to-elisp
  "((.9 abc (true) (hello)))"
  '(((0.9 abc (t) (hello)))))

(edn-parse-deftest elisp-strings-1
  edn-parse-to-elisp
  "\"abc hello \\t\\\"x\""
  '("abc hello \t\"x"))

(edn-parse-deftest elisp-strings-2
  edn-parse-to-elisp
  "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")"
  '(("---\f---\"-''-\\-\r\n")))

(edn-parse-deftest elisp-chars-1
  edn-parse-to-elisp
  "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)"
  '((?\n ?\r ?\ ?\t ?a ?b ?c ?x ?y)))

(edn-parse-deftest elisp-chars-2
  edn-parse-to-elisp
  "\"\\u0078 \\o171\""
  '("x y"))

(edn-parse-deftest elisp-keywords
  edn-parse-to-elisp
  ":foo-bar"
  '(:foo-bar))

(edn-parse-deftest elisp-vector
  edn-parse-to-elisp
  "[123]"
  '([123]))

(edn-parse-deftest elisp-map
  edn-parse-to-elisp
  "{:count 123}"
  '(((:count . 123))))

(edn-parse-deftest elisp-set
  edn-parse-to-elisp
  "#{:x}"
  '((:x)))

(edn-parse-deftest elisp-discarded
  edn-parse-to-elisp
  "(10 #_11 12 #_#_ 13 14)"
  '((10 12)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Clojure/EDN string

(edn-parse-deftest string-simple-list
  edn-parse-to-string
  "(   1 2   3)"
  "(1 2 3)")

(edn-parse-deftest string-empty-list
  edn-parse-to-string
  "()"
  "()")

(edn-parse-deftest string-list-size-1
  edn-parse-to-string
  "(1)"
  "(1)")

(edn-parse-deftest string-leafs
  edn-parse-to-string
  "(nil true false hello-world)"
  "(nil true false hello-world)")

(edn-parse-deftest string-qualified-symbol
  edn-parse-to-string
  "clojure.string/join"
  "clojure.string/join")

(edn-parse-deftest string-nested-lists
  edn-parse-to-string
  "((.9 abc (true) (hello)))"
  "((.9 abc (true) (hello)))")

(edn-parse-deftest string-strings-1
  edn-parse-to-string
  "\"abc hello \\t\\\"x\""
  "\"abc hello \\t\\\"x\"")

(edn-parse-deftest string-strings-2
  edn-parse-to-string
  "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")"
  "(\"---\\f---\\\"-'\\'-\\\\-\\r\\n\")")

(edn-parse-deftest string-chars-1
  edn-parse-to-string
  "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)"
  "(\\newline \\return \\space \\tab \\a \\b \\c \\u0078 \\o171)")

(edn-parse-deftest string-chars-2
  edn-parse-to-string
  "\"\\u0078 \\o171\""
  "\"\\u0078 \\o171\"")

(edn-parse-deftest string-keywords
  edn-parse-to-string
  ":foo-bar"
  ":foo-bar")

(edn-parse-deftest string-vector
  edn-parse-to-string
  "[123]"
  "[123]")

(edn-parse-deftest string-map
  edn-parse-to-string
  "{:count 123}"
  "{:count 123}")

(edn-parse-deftest string-set
  edn-parse-to-string
  "#{:x}"
  "#{:x}")

(edn-parse-deftest string-discarded
  edn-parse-to-string
  "(10 #_11 12 #_#_ 13 14)"
  "(10 12)")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST

(defun pretty-sort-node (node)
  (setf node (reverse node))
  (if (alist-get 'subnodes node)
      (setf (alist-get 'subnodes node)
            (mapcar 'pretty-sort-node (alist-get 'subnodes node))))
  node)

(defun edn-parse-and-pretty-sort-nodes ()
  (pretty-sort-node (edn-parse)))

(edn-parse-deftest ast-simple-list
  edn-parse-and-pretty-sort-nodes
  "(1 2 [3])"
  '((type . :root)
    (subnodes . (((pos . 1)
                  (type . :list)
                  (subnodes . (((pos . 2) (form . "1") (type . :number))
                               ((pos . 4) (form . "2") (type . :number))
                               ((pos . 6)
                                (type . :vector)
                                (subnodes . (((pos . 7) (form . "3") (type . :number))))))))))))


(provide 'edn-parse-test)

;;; edn-parse-test.el ends here
