#lang racket
;; pattern matching enables us to model phonological rules well
;; but there's gonna be a lot of boilerplate
;; SOURCES OF BOILERPLATE:
;;   * To capture simple but general rules, we may need to write out lots of cases
;;     SOLUTION: In another implementation, convert phonemes to bundles of features (structs)

;; TO DO:
;; * figure out struct representation of phonemes
;; * make struct representation of phonemes work in macros
;;   * fields of struct are distinctive features

;; TO FIX:
;; these macros need the parens (weird annoying things about ellipses...)
;; currently the in and out parameters need to be quoted
;; this is not necessarily a problem: eventually we will be representing phonemes as structs named for the phoneme
;; EX: (rule a -> o / (b) self (b))
;; Fix by: Removing need for parens
;; Fixed for non-list contexts!

;(define-syntax rule
;  (syntax-rules (-> / self)
;    [(_ in -> out / (lcontext ...) self (rcontext ...))
;     (λ (phonemes)
;       (match phonemes
;         [(list x ___ (quote lcontext) ... (quote in) (quote rcontext) ... y ___)
;          (flatten (list x (quote lcontext) ... (quote out) (quote rcontext) ... y))]
;         [else phonemes]))]))

;; note: you can have deletion rules--just use () for out
;; EX: ((rule r -> () / self .) '(k ah r .)) => '(k ah .)
;; Is there a way to condense this into less pattern-matching code?
;(define-syntax rule
;  (syntax-rules (-> / self)
;    [(_ (in ...) -> (out ...) / lcontext ... self rcontext) ; linguists recognize the notation: in becomes out in the context of lcontext _ rcontext
;     (λ (phonemes)
;       (match phonemes
;         [(list x ___ 'lcontext ... 'in ... 'rcontext y ___)
;          (flatten (list x 'lcontext ... 'out ... 'rcontext y))] 
;         [else phonemes]))]
;    [(_ (in ...) -> (out ...) / lcontext self) ; EX: retroflexion in Swedish/Norwegian: ((rule l -> l~ / r self) '(p aa r l o r)) => '(p aa r l~ o r)
;     (λ (phonemes)
;       (match phonemes
;         [(list x ___ 'lcontext 'in ... y ___)
;          (flatten (list x 'lcontext 'out ... y))] 
;         [else phonemes]))]
;    [(_ (in ...) -> (out ...) / self rcontext) ; EX: non-rhoticity: ((rule r -> () / self d) '(k aa r d)) => '(k aa d)
;     (λ (phonemes)
;       (match phonemes
;         [(list x ___ 'in ... 'rcontext y ___)
;          (flatten (list x 'out ... 'rcontext y))] 
;         [else phonemes]))]
;    [(_ (in ...) -> (out ...)) ; for completeness' sake, and also to allow you to model sound change
;     (λ (phonemes)
;       (match phonemes
;         [(list x ___ 'in ... y ___)
;          (flatten (list x 'out ... y))]
;         [else phonemes]))]))

(define-syntax rule
  (syntax-rules (-> / self)
    [(_ (in ...) -> (out ...) / (lcontext ...) self (rcontext ...)) ; linguists recognize the notation: in becomes out in the context of lcontext _ rcontext
     (λ (phonemes)
       (match phonemes
         [(list x ___ 'lcontext ... 'in ... 'rcontext ... y ___)
          (flatten (list x 'lcontext ... 'out ... 'rcontext ... y))] 
         [else phonemes]))]))


(define-syntax-rule (define-rule name : in -> out / (lcontext ...) self (rcontext ...))
  (define name (rule in -> out / (lcontext ...) self (rcontext ...))))

(define apply-rule
  (λ (rule phonemes)
    (do ((prev null out)
         (out phonemes (rule out)))
      ((equal? out prev) out)))) ; output will reduce no further

;; applies rules to phonemes in order they're listed
;; folds are wondrous -- here foldl allows us to use the list of rules as a pipeline
;; [[[runtime-generated] function] pipelining] is awesome!
;; EX:
;; (apply-rules
;;   (list (rule p -> b / a self a)
;;         (rule b -> v / a self a)
;;         (rule v -> w / a self a))
;;   '(a p a))
;;     => '(a w a)
(define apply-rules
  (λ (phonemes rules)
    (foldl apply-rule phonemes rules)))

(define translate
  (λ (w)
    (case w
      [(".") null] ; this version eliminates periods -- no explicit word boundaries
      ;[(".") 'end] ; this version keeps periods -- explicit word boundaries
      [else (string->symbol (string-downcase w))])))

(define line->symbols
  (λ (line)
    (map translate (string-split line " "))))

(define file->alist
  (λ (path)
    (map line->symbols (file->lines path))))

;; if a word is not in dict, it will be flattened out by sentence->phonemes
(define word->phonemes
  (λ (w dict)
    (let ((entry (assoc w dict)))
      (if entry
          (cdr entry)
          null))))

;; a sentence is a list of symbols (where each symbol is a key in dict)
;; with CMU Dictionary, will create a phonetic stream (no word boundaries)
(define sentence->phonemes
  (λ (sentence dict)
    (append-map (λ (s) (word->phonemes s dict)) sentence)))

(define *dictionary* (file->alist "cmu_dict_stressless.txt"))
;; this is a toy set of rules
;; it is, if anything, an indicator that we need to use the distinctive features of a phoneme in some way to have a workable phonologizer
(define *rules* (list (rule (n) -> (m) / () self (b)) ; part of a homorganic nasal rule in all English dialects
                      (rule (n) -> (ng) / () self (g)) ; homorganic nasals
                      (rule (ah) -> (ah r) / () self (ih)) ; small part of an r-insertion rule common in non-rhotic dialects of English
                      (rule (t r) -> (ch r) / () self ()) ; part of an affrication rule in many English dialects
                      (rule (d r) -> (jh r) / () self ()))) ; other part of affrictation rule

(define sentence->surface-form
  (λ (sentence)
    (apply-rules (sentence->phonemes sentence *dictionary*) *rules*)))

;; (sentence->phonemes '(argentina is a place where dreams can be real) *dictionary*) =>
;; '(aa r jh ah n t iy n ah ih z ah p l ey s w eh r d r iy m z k ae n b iy r iy l)
;; (sentence->surface-form '(argentina is a place where dreams can be real)) =>
;; '(aa r jh ah n t iy n ah r ih z ah p l ey s w eh r d jh r iy m z k ae m b iy r iy l)