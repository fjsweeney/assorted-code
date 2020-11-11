#lang racket

(require net/url)
(require pict)

; originally, types were auto-replaced with the pict version, but this was too terribly memory inefficient.
; instead, just associate the type with the pict version and look it up when needed.
(define type-space (make-empty-namespace))
(define colored-names '())
(define-syntax-rule (define-type name color)
  (begin
    (define name 'name) ;you can replace this with the pict version but it uses tons of memory due to the long list of type combos.
    (set! colored-names (cons (cons name
                                    (cc-superimpose
                                     (colorize (filled-rectangle 160 30) color)
                                     (colorize (text (symbol->string 'name) '(bold . default) 24) '(0 0 0))))
                              colored-names))
    (namespace-set-variable-value! 'name name #t type-space #t)))
(define-type Normal '(193 193 193))
(define-type Fire '(255 0 0))
(define-type Water '(0 0 255))
(define-type Electric '(255 255 0))
(define-type Grass '(0 255 0))
(define-type Ice '(0 255 255))
(define-type Fighting '(128 0 0))
(define-type Poison '(128 0 128))
(define-type Ground '(193 128 64)) 
(define-type Flying '(128 128 255))
(define-type Psychic '(255 0 255))
(define-type Bug '(128 128 0))
(define-type Rock '(128 64 0))
(define-type Ghost '(64 64 128))
(define-type Dragon '(0 0 128))
(define-type Dark '(64 64 64))
(define-type Steel '(128 128 128))
(define-type Fairy '(255 128 128))
(namespace-set-variable-value! 'Attacking (void) #t type-space #t) ; the chart has this in the top left and it needs to be ignored.

(define chart-url (string->url "https://raw.githubusercontent.com/robinsones/pokemon-chart/master/chart.csv"))
; 18 strings in a list
(define chart-strings
   (map (curryr string-split ",")
        (call/input-url chart-url get-pure-port
                        (lambda (in) (build-list 19 (thunk* (read-line in)))))))

; a matrix of type-effectiveness (19 by 19 due to type-annotations)
(define chart
  (map (curry map (compose1
                   (lambda (n) (cond ;this function exists to transform the symbols in the chart automagically to the pict versions
                                 ([symbol? n] (namespace-variable-value n #t #f type-space))
                                 (#t n))) 
                           read open-input-string))
       chart-strings))

;attackType defenseType(s)... -> (type-effectiveness of attack type against defense type(s))
(define (query-chart attack . defenses)
  (let ([attack-row (list-ref chart (index-of (car chart) attack))])
    (apply * (map (lambda (defense)
                    (list-ref attack-row (index-of (car chart) defense)))
                  defenses))))

;attackType defenseType(s)... -> (type-effectiveness list against this combination)
(define (defense-of . types)
  (define (lookup-one-defense type)
    (cdr (map (curryr list-ref (index-of (car chart) type)) chart)))
  (apply (curry map *) (map lookup-one-defense types)))

;Set of (Sets of 1 or 2 types)
(define valid-combos
  (set-subtract
   (list->set
    (append (map list->set (combinations (cdar chart) 2))
            (map set (cdar chart))))
   (set (set Normal Ghost)
        (set Normal Ice)
        (set Normal Bug)
        (set Normal Poison)
        (set Normal Rock)
        (set Normal Steel)
        (set Fire Grass)
        (set Fire Fairy)
        (set Electric Fighting)
        (set Ice Poison)
        (set Fighting Ground)
        (set Fighting Fairy)
        (set Poison Steel)
        (set Ground Fairy)
        (set Bug Dragon)
        (set Bug Dark)
        (set Rock Ghost))))
       
(define (sum-defense . types)
  (apply + (apply defense-of types)))

(define (sum-square-defense . types)
  (apply + (map sqr (apply defense-of types))))

;List of sets of types -> float (a rating)
(define (eval-team-v1 team)
  (apply +
         (apply (curry map *)
         (map (curry apply defense-of) team))))

(define valid-teams
  (in-combinations (set->list valid-combos) 6))

(define (best-team)
  (define (team-comparer oldteam newteam)
    (let ([newscore (eval-team-v1 (map set->list newteam))])
      (displayln oldteam)
      (cond
        ([< newscore (car oldteam)]
         (displayln (cons newscore (map set->list newteam)))
         (cons newscore (map set->list newteam)))
        (#t oldteam))))
  (sequence-fold team-comparer (cons 10000 '()) valid-teams))
(best-team)