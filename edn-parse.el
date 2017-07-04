(require 's)
(require 'let-alist)
(require 'cl-lib)
(require 'clj-lex)

(require 'edn-string-utils)

(defvar edn-parse--opener-tokens '(:root
                                   :lparen
                                   :lbracket
                                   :lbrace
                                   :set
                                   :discard)
  "Tokens that represent opener nodes in the AST.")

(defvar edn-parse--leaf-tokens '(:number
                                 :nil
                                 :true
                                 :false
                                 :symbol
                                 :keyword
                                 :string
                                 :character)
  "Tokens that represent leaf nodes in the AST.")

(defvar edn-parse--closer-tokens '(:eof  ; not really a closer, but acts like one.
                                   :rparen
                                   :rbracket
                                   :rbrace)
  "Tokens that represent closing of an AST branch.")

(defun edn-parse--next ()
  (setq next (clj-lex-next))
  (while (eq (edn-parse--get-type next) :whitespace)
    (setq next (edn-parse--next)))
  next)

(defun edn-parse--get-type (el)
  (and (listp el)
       (cdr (assq 'type el))))

(defun edn-parse--is-opener (el)
  (member (edn-parse--get-type el) edn-parse--opener-tokens))

(defun edn-parse--is-leaf (el)
  (member (edn-parse--get-type el) edn-parse--leaf-tokens))

(defun edn-parse--is-closer (el)
  (member (edn-parse--get-type el) edn-parse--closer-tokens))

(defun edn-parse--is-discard (el)
  (eq (edn-parse--get-type el) :discard))

(defun edn-parse--make-opener (node)
  "Renames the `node` type if it's a :list, :map or a :vector,
removes the `form` key and adds a `subnodes` keys to track
subnodes."
  (let ((node-type (edn-parse--get-type node)))
    (push (cons 'subnodes '()) node)
    (setf (alist-get 'type node)
          (cl-case node-type
            (:lparen :list)
            (:lbracket :vector)
            (:lbrace :map)
            (t node-type)))
    (assq-delete-all 'form node)))

(defun edn-parse--get-consumption-by-type (node)
  "Number of nodes that should be consumed after the given
`node`, useful for special nodes like :discard
and :tagged-literal"
  (1+ (cl-case (edn-parse--get-type node)
        (:discard 1)
        (t -1))))

(defun edn-parse-reduction (node)
  (if (not (edn-parse--is-opener node))
      node
    (let ((node (edn-parse--make-opener node)))  ; reduce children
      (setq consume (edn-parse--get-consumption-by-type node))
      (while (not (or (eq (setq consume (- consume 1)) 0)
                      (edn-parse--is-closer (setq subnode (edn-parse-reduction (edn-parse--next))))))
        (push subnode (alist-get 'subnodes node)))
      (setf (alist-get 'subnodes node) (reverse (alist-get 'subnodes node)))  ; reverse the subnodes list
      node)))

(defun edn-parse ()
  "Returns an AST structure to be used to evaluate/print. Nodes
follow the same structure as tokens, but include a 'subnodes list
for subnodes."
  (edn-parse-reduction `((type . :root))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Elisp

(defun edn-parse--reduce-elisp-leaf (leaf)
  (cl-case (edn-parse--get-type leaf)
    (:number (string-to-number (alist-get 'form leaf)))
    (:nil nil)
    (:true t)
    (:false nil)
    (:symbol (intern (alist-get 'form leaf)))
    (:keyword (intern (alist-get 'form leaf)))
    (:string (edn-parse--string (alist-get 'form leaf)))
    (:character (edn-parse--character (alist-get 'form leaf)))))

(defun edn-parse--reduce-to-elisp (node)
  (if (edn-parse--is-leaf node)
      (edn-parse--reduce-elisp-leaf node)
    (let ((subnodes (-remove 'edn-parse--is-discard (alist-get 'subnodes node))))
      (cl-case (edn-parse--get-type node)
        (:root (mapcar 'edn-parse--reduce-to-elisp subnodes))
        (:list (mapcar 'edn-parse--reduce-to-elisp subnodes))
        (:vector (apply #'vector (mapcar 'edn-parse--reduce-to-elisp subnodes)))
        (:set (mapcar 'edn-parse--reduce-to-elisp subnodes))
        (:map (mapcar (lambda (pair)
                        (cons (edn-parse--reduce-to-elisp (car pair))
                              (edn-parse--reduce-to-elisp (cadr pair))))
                      (-partition 2 subnodes)))
        ;; tagged literal
        ))))

(defun edn-parse-to-elisp ()
  (edn-parse--reduce-to-elisp (edn-parse)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Clojure/EDN string

(defun edn-parse--reduce-string-leaf (leaf)
  (alist-get 'form leaf))

(defun edn-parse--string-with-delimiters (nodes ld rd)
  (s-concat ld
            (s-join " " (mapcar 'edn-parse--reduce-to-string nodes))
            rd))

(defun edn-parse--reduce-to-string (node)
  (if (edn-parse--is-leaf node)
      (edn-parse--reduce-string-leaf node)
    (let ((subnodes (-remove 'edn-parse--is-discard (alist-get 'subnodes node))))
      (cl-case (edn-parse--get-type node)
        (:root (edn-parse--string-with-delimiters subnodes "" ""))
        (:list (edn-parse--string-with-delimiters subnodes "(" ")"))
        (:vector (edn-parse--string-with-delimiters subnodes "[" "]"))
        (:set (edn-parse--string-with-delimiters subnodes "#{" "}"))
        (:map (edn-parse--string-with-delimiters subnodes "{" "}"))
        ;; tagged literals
        ))))

(defun edn-parse-to-string ()
  (edn-parse--reduce-to-string (edn-parse)))

(provide 'edn-parse)
