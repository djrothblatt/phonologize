#lang racket
;; pattern matching enables us to model phonological rules well
;; but there's gonna be a lot of boilerplate
;; SOURCES OF BOILERPLATE:
;;   * To capture simple but general rules, we may need to write out lots of cases
;;     SOLUTION: Convert phonemes to bundles of features (structs)

;; TO DO:
;; * figure out struct representation of phonemes
;;   * fields of struct are distinctive features
;; * make struct representation of phonemes work in pattern-matching macros

;; no quoting necessary: (rule (a) -> (b) / () self ())
(define-syntax rule
  (syntax-rules (-> / self)
    [(_ (in ...) -> (out ...) / (lcontext ...) self (rcontext ...)) ; linguists recognize the notation: in becomes out in the context of lcontext _ rcontext
     (λ (phonemes)
       (match phonemes
         [(list x ___ 'lcontext ... 'in ... 'rcontext ... y ___)
          (flatten (list x 'lcontext ... 'out ... 'rcontext ... y))] 
         [else phonemes]))]))


(define-syntax-rule (define-rule name : (in) -> (out) / (lcontext ...) self (rcontext ...))
  (define name (rule (in) -> (out) / (lcontext ...) self (rcontext ...))))

(define apply-rule
  (λ (rule phonemes)
    (do ((prev null out)
         (out phonemes (rule out)))
      ((equal? out prev) out)))) ; output will reduce no further

;; applies rules to phonemes in order rules are listed
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

;; either keeps or drops word boundaries and sets all input strings to lowercase
;; bounded? is a boolean that tells us whether to keep or drop word boundaries
(define translate
  (λ (bounded?)
    (λ (w)
      (if (eq? w ".")
          (if bounded? 'end null)
          (string->symbol (string-downcase w))))))
  
(define translate/bounded (translate #t))
(define translate/unbounded (translate #f))

;; turns a line of input into a list of symbols
;; ARPAbet uses spaces to separate phonemes, so this will break a line up into a list of phonemes with their word at the front
;; (in other words, this will turn a line into a cell of an association list)
(define line->symbols
  (λ (line)
    (map translate/unbounded (string-split line " "))))

(define file->alist
  (λ (path)
    (map line->symbols (file->lines path))))

;; looks w up in dict and evaluates to the associated list of phonemes if found
;; if a word is not in dict, it will be flattened out by sentence->phonemes
(define word->phonemes
  (λ (w dict)
    (let ((entry (assoc w dict)))
      (if entry
          (cdr entry)
          null))))

;; a sentence is a list of symbols (where each symbol is a key in dict)
;; with CMU Dictionary, will create a phonetic stream
(define sentence->phonemes
  (λ (sentence dict)
    (append-map (λ (s) (word->phonemes s dict)) sentence)))

(define *dictionary* (file->alist "cmu_dict_stressless.txt"))
;; this is a toy set of rules
;; it is, if anything, an indicator that we need to use the distinctive features of a phoneme in some way to have a workable phonologizer
(define *rules* (list (rule (n) -> (m) / () self (b)) ; part of a homorganic nasal rule in all English dialects
                      (rule (n) -> (ng) / () self (g)) ; more homorganic nasals
                      (rule (ah) -> (ah r) / () self (ih)) ; small part of an r-insertion rule common in non-rhotic dialects of English
                      (rule (t r) -> (ch r) / () self ()) ; part of an affrication rule in many English dialects
                      (rule (d r) -> (jh r) / () self ()))) ; other part of affrictation rule

;; creates phonemic stream and then applies the rules to it
(define sentence->surface-form
  (λ (sentence)
    (apply-rules (sentence->phonemes sentence *dictionary*) *rules*)))

;; compare these outputs to see how surface forms differ from underlying forms
;; > (sentence->phonemes '(trolls are ungainly and dragons can be unbearable) *dictionary*)
;; '(t r ow l z aa r ah n g ey n l iy ah n d d r ae g ah n z k ae n b iy ah n b eh r ah b ah l)
;; > (sentence->surface-form '(trolls are ungainly and dragons can be unbearable))
;; '(ch r ow l z aa r ah ng g ey n l iy ah n d jh r ae g ah n z k ae m b iy ah m b eh r ah b ah l)
