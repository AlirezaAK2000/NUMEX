;; PL Project - Fall 2021
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
(require racket/set)
;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  
(struct num  (int)    #:transparent)  
(struct bool (b)      #:transparent)  
(struct plus  (e1 e2)  #:transparent) 
(struct minus  (e1 e2)  #:transparent)
(struct mult  (e1 e2)  #:transparent) 
(struct div  (e1 e2)  #:transparent)  
(struct neg (e1)      #:transparent)  
(struct andalso  (e1 e2)  #:transparent)
(struct orelse  (e1 e2)  #:transparent) 
(struct cnd  (e1 e2 e3)  #:transparent) 
(struct iseq  (e1 e2)  #:transparent)  
(struct ifnzero  (e1 e2 e3)  #:transparent)  
(struct ifleq  (e1 e2 e3 e4)  #:transparent) 
(struct with  (s e1 e2)  #:transparent)
(struct apair  (e1 e2)  #:transparent) 
(struct 1st  (e1)  #:transparent) 
(struct 2nd  (e1)  #:transparent) 

(struct lam  (s1 s2 e) #:transparent) 
(struct tlam  (s1 s2 arg-type e) #:transparent)
(struct apply (e1 e2)       #:transparent) 


(struct munit   ()      #:transparent) 
(struct ismunit (e)     #:transparent) 

(struct closure (env f) #:transparent) 


(struct key  (s e) #:transparent) 
(struct record (k r) #:transparent) 
(struct value (s r) #:transparent) 

(struct letrec (s1 e1 s2 e2 s3 e3 s4 e4 e5) #:transparent) 

;; Type structures
;; Primitive types are: "int", "bool" and "null"
(struct collection (type) #:transparent) ;; collection of a certain type, e.g., (collection "int")
(struct function (input-type output-type) #:transparent) ;; e.g. (function ("int" int")) means fn f "int" -> "int"

;; Problem 1

(define (racketlist->numexlist xs)(cond 
  [(null? xs) (munit)]
  [(list? xs) (apair (car xs) (racketlist->numexlist (cdr xs)))]
))
(define (numexlist->racketlist xs)(cond 
  [(munit? xs) null]
  [(apair? xs) (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs))) ]
))

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
    [(not (string? str)) (error ("str is not a string"))]
    [(not (list? env)) (error ("env structure is not compatibale"))]
    [(equal? str (car (car env))) (cdr (car env))]
    [true (envlookup (cdr env) str)]
	)
)


;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
       
        
        [(num? e) (cond 
          [(integer? (num-int e)) e ]
          [true (error "NUMEX var applied to non-racket-string")]
        )]

        [(bool? e)
        (cond[(boolean? (bool-b e)) e]
          [true (error "NUMEX bool applied to non-racket-boolean")]
        )]
        
        
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        
        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX minus applied to non-number")))]
        
        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]
        
        [(div? e) 
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (quotient (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        
        [(neg? e) 
         (let ([v1 (eval-under-env (neg-e1 e) env)])
           (cond [(num? v1) (num (- 0 (num-int v1)))] 
                 [(bool? v1)
                  (cond [(equal? (bool-b v1) #t) (bool #f)]
                       [(equal? (bool-b v1) #f) (bool #t)])] 
                 [true (error "NUMEX negation applied to non-number")]))]

        [(andalso? e) 
         (let ([v1 (eval-under-env (andalso-e1 e) env)])
           (cond [ (bool? v1) (cond [(not (bool-b v1)) (bool #f)]

                                [true (let ([v2 (eval-under-env (andalso-e2 e) env)])
                                      (cond [(bool? v2) v2]
                                           [true (error "NUMEX conjunction applied to non-number")]))])]
                [true (let ([v2 (eval-under-env (andalso-e2 e) env)])
                      (cond [(bool? v2) v2]
                           [true (error "NUMEX conjunction applied to non-number")]))]))]

        [(orelse? e) 
         (let ([v1 (eval-under-env (orelse-e1 e) env)])
           (cond [(bool? v1)(cond [(bool-b v1)(bool #t) ]
              [true (let ([v2 (eval-under-env (orelse-e2 e) env)])
                (cond [(bool? v2) v2]
                      [true (error "NUMEX conjunction applied to non-number")]))])]
              [true (let ([v2 (eval-under-env (orelse-e2 e) env)])
                (cond [(bool? v2) v2]
                      [true (error "NUMEX disjunction applied to non-number")]))]))]

        [(cnd? e)
         (let ([v (eval-under-env (cnd-e1 e) env)])
           (if (bool? v) 
               (cond [(equal? (bool-b v) true) (eval-under-env (cnd-e2 e) env)]
                 [true (eval-under-env (cnd-e3 e) env)])
               (error "NUMEX cnd applied to a non-boolean condition")))]

        [(iseq? e) 
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
           (cond [(and (or (bool? v1) (num? v1))(or (bool? v2) (num? v2)))(cond [(and (bool? v1) (bool? v2))(cond [(equal? (bool-b v1) (bool-b v2))(bool #t)]
                                                [true (bool #f)])]
              [(and (num? v1) (num? v2))(cond [(equal? (num-int v1) (num-int v2))(bool #t)]
                                              [true (bool #f)])]
              [true (bool #f)])]
              [true (error "NUMEX equality is applied to something other than non-booleans or non-numbers")]))]


        [(ifleq? e) 
         (let ([v1 (eval-under-env (ifleq-e1 e) env)]
               [v2 (eval-under-env (ifleq-e2 e) env)])
           (if (and (num? v1) (num? v2))
               (cond [(> (num-int v1) (num-int v2))(let ([v4 (eval-under-env (ifleq-e4 e) env)]) v4)]
                    [(not (> (num-int v1) (num-int v2)))(let ([v3 (eval-under-env (ifleq-e3 e) env)]) v3)])
           (error "NUMEX ifleq condition applied to non-numbers")))]


        [(with? e)
         (let ([v1 (eval-under-env (with-e1 e) env)])
           (cond [(string? (with-s e)) (eval-under-env (with-e2 e) (cons (cons (with-s e) v1) env))]
                [(null?   (with-s e)) (eval-under-env (with-e2 e) (cons v1 env))]
                [true (error "NUMEX with applied to non-string")]))]

        [(apply? e)
          (let ([func (eval-under-env (apply-e1 e) env)]) 
            (cond 
              [(closure? func) (let ([fun-def (closure-f func)]) 
                (let (
                  [eval-e2 (eval-under-env (apply-e2 e) env)]
                )
                (cond 
                  [(lam? fun-def) (
                    eval-under-env (lam-e fun-def) 
                    (cons (cons (lam-s2 fun-def) eval-e2)
                    (cons (cons (lam-s1 fun-def) func) (closure-env func)))
                  )
                  ] 
                  [true (error "closure function isn't lam")])
                )
              )
              ]

              [(lam? func) 
              (let (
                [clos (eval-under-env func env)]
                ) 
                (let (
                  [lam-def (closure-f clos)]
                  [eval-e2 (eval-under-env (apply-e2 e) env)]
                )
                      (cond [(lam? lam-def)(eval-under-env (lam-e lam-def) (cons (cons (lam-s2 lam-def) eval-e2)
                                                                  (cons (cons (lam-s1 lam-def) clos) (closure-env clos))))] 
                          [true (error "closure function isn't lam")])))]
              
              [true (error (format "function type error in apply : ~v" e))]
              )
            )
          ]
        
        
        [(lam? e)
          (closure env e)]


        [(apair? e) 
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))] 

        [(1st? e) 
         (let ([v1 (eval-under-env (1st-e1 e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "NUMEX 1st applied to non-apair")))]

        [(2nd? e) 
         (let ([v1 (eval-under-env (2nd-e1 e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "NUMEX 2nd applied to non-apair")))]

        [(munit? e) (munit)] 

        [(ismunit? e)
         (let ([v1 (eval-under-env (ismunit-e e) env)])
           (cond[(munit? v1)(bool #t)]
                [else (bool #f)]))]

        [(closure? e) e] 

        [(letrec? e)
         (let ([e1 (letrec-e1 e)]
               [s1 (letrec-s1 e)]
               [e2 (letrec-e2 e)]
               [s2 (letrec-s2 e)]
               [e3 (letrec-e3 e)]
               [s3 (letrec-s3 e)]
               [e4 (letrec-e4 e)]
               [s4 (letrec-s4 e)]
               [e5 (letrec-e5 e)])
         (if (and (string? (letrec-s1 e)) (string? (letrec-s2 e)) (string? (letrec-s3 e)) (string? (letrec-s4 e)))
             (eval-under-env e5 (cons (cons s1 e1)(cons (cons s2 e2)(cons (cons s3 e3) (cons (cons s4 e4) env)))))
             (error "NUMEX letrec applied to non-string")))]


        [(key? e)
         (let ([ex (eval-under-env (key-e e) env)])
               (cond[(string? (key-s e)) e]
                    [else (error "key is not a string")]))]

        [(record? e)
         (let ([k (eval-under-env (record-k e) env)]
               [r (eval-under-env (record-r e) env)])
               (cond[(key? k) (cond[(or (munit? (eval-exp r)) (record? r)) (record k r)]
                                   [else (error "value of record invalid")])]
                [else (error "key invalid")]))]

        [(value? e)
          (let ([v2 (eval-under-env (value-r e) env)])
            (cond [(string? (value-s e)) (cond [(record? v2) (cond [(equal? (value-s e) (key-s (record-k v2))) (key-e (record-k v2))]
                                                          [(munit? (record-r v2)) (munit)]
                                                          [true (eval-under-env (value (value-s e) (record-r v2)) null)])]
                                      [true (error "Second value argument is not a record")])]
                  [true (error "First value argument is not a string")]))]

        [(ifnzero? e) 
         (let ([v1 (eval-under-env (ifnzero-e1 e) env)])
           (if (num? v1)
               (cond[(not (equal? (num-int v1) 0))(let ([v2 (eval-under-env (ifnzero-e2 e) env)]) v2)]
                    [(equal? (num-int v1) 0)(let ([v3 (eval-under-env (ifnzero-e3 e) env)]) v3)])
               (error "NUMEX ifnotzero condition applied to non-number")))]

        

        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))


        

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3
;; Complete more cases for other kinds of NUMEX expressions.
;; We will test infer-under-env by calling its helper function, infer-exp.
(define (infer-under-env e env)
  (cond [(var? e) 
         (infer-under-env (envlookup env (var-string e)) env)]

        [(munit? e) "null" ]

        [(num? e)
         (cond
           [(integer? (num-int e)) "int"]
           [#t (error "NUMEX TYPE ERROR: num should be a constant number")])]

        [(bool? e)
         (cond
           [(boolean? (bool-b e)) "bool"]
           [#t (error "NUMEX TYPE ERROR: bool should be #t or #f")])]

        [(plus? e) 
         (let ([t1 (infer-under-env (plus-e1 e) env)]
               [t2 (infer-under-env (plus-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: addition applied to non-integer")))]

        [(minus? e) 
         (let ([t1 (infer-under-env (minus-e1 e) env)]
               [t2 (infer-under-env (minus-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: subtraction applied to non-integer")))]

        [(div? e) 
         (let ([t1 (infer-under-env (div-e1 e) env)]
               [t2 (infer-under-env (div-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: division applied to non-integer")))]

        [(mult? e) 
         (let ([t1 (infer-under-env (mult-e1 e) env)]
               [t2 (infer-under-env (mult-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: multiplication applied to non-integer")))]
        
        [(andalso? e) 
         (let ([t1 (infer-under-env (andalso-e1 e) env)]
              [t2 (infer-under-env (andalso-e2 e) env)])
            (if (and (equal? "bool" t1)
                  (equal? "bool" t2))
              "bool"
              (error "NUMEX TYPE ERROR: AND applied to non-boolean")))]

        [(orelse? e) 
         (let ([t1 (infer-under-env (orelse-e1 e) env)]
              [t2 (infer-under-env (orelse-e2 e) env)])
            (if (and (equal? "bool" t1)
                  (equal? "bool" t2))
              "bool"
              (error "NUMEX TYPE ERROR: OR applied to non-boolean")))]

        [(neg? e)
          (let ([t1 (infer-under-env (neg-e1 e) env)])(
            if (or (equal? "int" t1) (equal? "bool" t1)) t1 (error ( format "NUMEX TYPE ERROR: negation applyied to non-integer or non-boolean ~v" e))
          ))
        ]

        [(cnd? e)
        (let 
          ([t1 (infer-under-env (cnd-e1 e) env)]
          [t2 (infer-under-env (cnd-e2 e) env)]
          [t3 (infer-under-env (cnd-e3 e) env)])
        (if (equal? t1 "bool") 
          (if (equal? t2 t3) t2 (error "NUMEX TYPE ERROR: the types of branches are not equal"))
          (error "NUMEX TYPE ERROR: type of the condition expression must be boolean")
        )) 
        ]

        [(iseq? e)
        (let (
          [t1 (infer-under-env (iseq-e1 e) env)]
          [t2 (infer-under-env (iseq-e2 e) env)]
        ) (if (equal? t1 t2) "bool" (error "NUMEX TYPE ERROR: the types of operands are not equal")))
        ]

        [(with? e)
        (let (
          [t1 (infer-under-env (with-e1 e) env)]
        )(infer-under-env (with-e2 e) (cons (cons (with-s e) t1) env))
        )
        ]

        [(tlam? e)
          (function (tlam-arg-type e) (infer-under-env (tlam-e e) (cons (cons (tlam-s2 e) (tlam-arg-type e)) env)))
        ]

        [(apply? e)(let
          ([t1 (infer-under-env (apply-e1 e) env)]
          [t2 (infer-under-env (apply-e2 e) env)])
          (
            if (function? t1) (
              if (equal? (function-input-type t1) t2)
              (function-output-type t1)
              (error "NUMEX TYPE ERROR: The type of input and declared argument are not equal")
            ) (error "NUMEX TYPE ERROR: expected function as first argument")
          ))
        ]

        [(apair? e)(let 
          ([t1 (infer-under-env (apair-e1 e) env)]
          [t2 (infer-under-env (apair-e2 e) env)])
        (cond
          [(collection? t2) (if (equal? t1 (collection-type t2)) t2 (error "NUMEX TYPE ERROR: the types of head and tail elements are not equal") ) ]
          [(equal? t2 "null") (collection t1) ]
          [true (error "NUMEX TYPE ERROR: the types of pair elements are not equal")]
        ))]
        
        [(1st? e)(let (
          [t (infer-under-env (1st-e1 e) env )]
        )(if (collection? t) (collection-type t) (error "NUMEX TYPE ERROR: input is not a collection") )
        )]

        [(2nd? e)(let (
          [t (infer-under-env (2nd-e1 e) env )]
        )(if (collection? t) t (error "NUMEX TYPE ERROR: input is not a collection") )
        )]

        [(ismunit? e)(let (
          [t (infer-under-env (ismunit-e e) env )]
        )(if (or (collection? t) (equal? t "null")) "bool" (error "NUMEX TYPE ERROR: "))
        )]

        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (infer-exp e)
  (infer-under-env e null))

;; Problem 4

(define (ifmunit e1 e2 e3)
  (cnd (ismunit e1) e2 e3))


(define (with* bs e2)
  (cond [(null? (car bs)) e2]
        [(null? (cdr bs)) (with (car (car bs)) (cdr (car bs)) e2)]
        [true (with (car (car bs)) (cdr (car bs)) (with* (cdr bs) e2))]))


(define (ifneq e1 e2 e3 e4)
  (cnd (iseq e1 e2) e4 e3))

;; Problem 5

(define numex-filter (lam null "f" (lam "mapNonZero" "lst" 
  (ifmunit (var "lst") (munit) (ifnzero (apply (var "f") (1st (var "lst")))
    (apair (apply (var "f") (1st (var "lst"))) (apply (var "mapNonZero") (2nd (var "lst"))))
    (apply (var "mapNonZero") (2nd (var "lst"))) )))))                                             

(define numex-all-gt
  (with "filter" numex-filter
       (lam null "number" (apply (var "filter") (lam null "x" (ifleq (var "x") (var "number") (num 0) (var "x")))))))


;; Problem 6

(define type-error-but-evaluates-ok (cnd (bool #t) (num 2) (bool #t)))
(define type-ok-but-evaluates-error (div (num 1) (num 0)))

;; Challenge Problem

(struct fun-challenge (s1 s2 e freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (car (find-free-vars e))
)

(define (find-free-vars e)
  (cond [(var? e) (cons e (set (var-string e)))]
        [(num? e) (cons e (set))]
        [(bool? e) (cons e (set))]
        
        
        [(plus? e) 
         (let ([e1 (find-free-vars (plus-e1 e))]
               [e2 (find-free-vars (plus-e2 e))])
           (cons e (set-union (cdr e2) (cdr e1))))]
        [(minus? e) 
         (let ([e1 (find-free-vars (minus-e1 e))]
               [e2 (find-free-vars (minus-e2 e))])
           (cons e (set-union (cdr e2) (cdr e1))))]
        [(mult? e) 
         (let ([e1 (find-free-vars (mult-e1 e))]
               [e2 (find-free-vars (mult-e2 e))])
           (cons e (set-union (cdr e2) (cdr e1))))]
        [(div? e) 
         (let ([e1 (find-free-vars (div-e1 e))]
               [e2 (find-free-vars (div-e2 e))])
           (cons e (set-union (cdr e2) (cdr e1))))]
           
        [(andalso? e)
         (let ([e1 (find-free-vars (andalso-e1 e))]
               [e2 (find-free-vars (andalso-e2 e))])
           (cons e (set-union (cdr e2) (cdr e1))))]
        [(orelse? e)
         (let ([e1 (find-free-vars (orelse-e1 e))]
               [e2 (find-free-vars (orelse-e2 e))])
           (cons e (set-union (cdr e2) (cdr e1))))]
           
        [(neg? e)
         (let ([v (find-free-vars (neg-e1 e))])
           (cons e (cdr v)))]
                 
        [(cnd? e)
         (let ([e1 (find-free-vars (cnd-e1 e))]
               [e2 (find-free-vars (cnd-e2 e))]
               [e3 (find-free-vars (cnd-e3 e))])
           (cons e (set-union (cdr e1) (cdr e2) (cdr e3))))]
               
        [(iseq? e)
         (let ([e1 (find-free-vars (iseq-e1 e))]
               [e2 (find-free-vars (iseq-e2 e))])
           (cons e (set-union (cdr e2) (cdr e1))))]
        
        [(ifnzero? e)
         (let ([e1 (find-free-vars (ifnzero-e1 e))]
               [e2 (find-free-vars (ifnzero-e2 e))]
               [e3 (find-free-vars (ifnzero-e3 e))])
           (cons e (set-union (cdr e1) (cdr e2) (cdr e3))))]
           
        [(ifleq? e)
         (let ([e1 (find-free-vars (ifleq-e1 e))]
               [e2 (find-free-vars (ifleq-e2 e))]
               [e3 (find-free-vars (ifleq-e3 e))]
               [e4 (find-free-vars (ifleq-e4 e))])
           (cons e (set-union (cdr e1) (cdr e2) (cdr e3) (cdr e4))))]
        
        [(with? e)
         (let ([e1 (find-free-vars (with-e1 e))]
               [e2 (find-free-vars (with-e2 e))])
            (cons (with (with-s e) (car e1) (car e2)) (set-union (set-remove (cdr e2) (with-s e)) (cdr e1))))]
        
        [(ismunit? e) 
         (let ([v (find-free-vars (ismunit-e e))])
           (cons e (cdr v)))]

        [(munit? e) (cons e (set))]

        [(key? e) (cons e (set))]
        
        [(closure? e) (cons e (set))]

        [(lam? e)
         (let ([body (find-free-vars (lam-e e))])
           (let ([free-var-set (set-remove (set-remove (cdr body) (lam-s2 e)) (lam-s1 e))])
               (cons (fun-challenge (lam-s1 e) (lam-s2 e) (car body) free-var-set) free-var-set)))]

        [(apply? e)
         (let ([v (find-free-vars (apply-e1 e))]
               [arg (find-free-vars (apply-e2 e))])
           (cons e (set-union (cdr v) (cdr arg))))]

        
        [(apair? e)
         (let ([e1 (find-free-vars (apair-e1 e))]
               [e2 (find-free-vars (apair-e2 e))])
           (cons e (set-union (cdr e1) (cdr e2))))]
        [(1st? e)
         (let ([v (find-free-vars (1st-e1 e))])
           (cons e (cdr v)))]
        [(2nd? e)
         (let ([v (find-free-vars (2nd-e1 e))])
           (cons e (cdr v)))]

        [(key? e)
          (let ([v (find-free-vars (key-e e))])
           (cons e (cdr v)))]
        [(record? e)
          (let ([v (find-free-vars (record-r e))])
           (cons e (cdr v)))]
         [(value? e)
          (let ([v (find-free-vars (value-s e))])
            (cons e (set-union (cdr v))))]

         [(letrec? e)
         (let ([e1 (find-free-vars (letrec-e1 e))]
               [e2 (find-free-vars (letrec-e2 e))]
               [e3 (find-free-vars (letrec-e3 e))]
               [e4 (find-free-vars (letrec-e4 e))]
               [e5 (find-free-vars (letrec-e5 e))]
               [s1 (letrec-s1 e)]
               [s2 (letrec-s2 e)]
               [s3 (letrec-s3 e)]
               [s4 (letrec-s4 e)]
               )
           (cons e (set-remove (set-remove (set-remove (set-remove (set-union (cdr e1) (cdr e2) (cdr e3) (cdr e4) (cdr e5)) s4) s3 ) s2 ) s1)))]

        [#t (error (format "bad NUMEX expression: ~v" e))]))
;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes

(define (find-free-vars-in-env env set)
  (cond [(equal? env null) null]
        [true (cond [(set-member? set (car (car env))) (cons (car env) (find-free-vars-in-env (cdr env) set))]
                    [true (find-free-vars-in-env (cdr env) set)])]))

(define (eval-under-env-c e env) (cond 
        [(var? e) 
          (envlookup env (var-string e))]
       
        
        [(num? e) (cond 
          [(integer? (num-int e)) e ]
          [true (error "NUMEX var applied to non-racket-string")]
        )]

        [(bool? e)
        (cond[(boolean? (bool-b e)) e]
          [true (error "NUMEX bool applied to non-racket-boolean")]
        )]
        
        [(fun-challenge? e)
         (let ([freevars (fun-challenge-freevars e)])
           (cond [(null? (fun-challenge-s1 e)) (closure (find-free-vars-in-env env freevars) e)]
                 [true (closure (find-free-vars-in-env (cons (cons (fun-challenge-s1 e) e) env) freevars) e)]))]
        
        [(plus? e) 
         (let ([v1 (eval-under-env-c (plus-e1 e) env)]
               [v2 (eval-under-env-c (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        
        [(minus? e) 
         (let ([v1 (eval-under-env-c (minus-e1 e) env)]
               [v2 (eval-under-env-c (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX minus applied to non-number")))]
        
        [(mult? e) 
         (let ([v1 (eval-under-env-c (mult-e1 e) env)]
               [v2 (eval-under-env-c (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]
        
        [(div? e) 
         (let ([v1 (eval-under-env-c (div-e1 e) env)]
               [v2 (eval-under-env-c (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (quotient (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        
        [(neg? e) 
         (let ([v1 (eval-under-env-c (neg-e1 e) env)])
           (cond [(num? v1) (num (- 0 (num-int v1)))] 
                 [(bool? v1)
                  (cond [(equal? (bool-b v1) #t) (bool #f)]
                       [(equal? (bool-b v1) #f) (bool #t)])] 
                 [true (error "NUMEX negation applied to non-number")]))]

        [(andalso? e) 
         (let ([v1 (eval-under-env-c (andalso-e1 e) env)])
           (cond [ (bool? v1) (cond [(not (bool-b v1)) (bool #f)]

                                [true (let ([v2 (eval-under-env-c (andalso-e2 e) env)])
                                      (cond [(bool? v2) v2]
                                           [true (error "NUMEX conjunction applied to non-number")]))])]
                [true (let ([v2 (eval-under-env-c (andalso-e2 e) env)])
                      (cond [(bool? v2) v2]
                           [true (error "NUMEX conjunction applied to non-number")]))]))]

        [(orelse? e) 
         (let ([v1 (eval-under-env-c (orelse-e1 e) env)])
           (cond [(bool? v1)(cond [(bool-b v1)(bool #t) ]
              [true (let ([v2 (eval-under-env-c (orelse-e2 e) env)])
                (cond [(bool? v2) v2]
                      [true (error "NUMEX conjunction applied to non-number")]))])]
              [true (let ([v2 (eval-under-env-c (orelse-e2 e) env)])
                (cond [(bool? v2) v2]
                      [true (error "NUMEX disjunction applied to non-number")]))]))]

        [(cnd? e)
         (let ([v (eval-under-env-c (cnd-e1 e) env)])
           (if (bool? v) 
               (cond [(equal? (bool-b v) true) (eval-under-env-c (cnd-e2 e) env)]
                 [true (eval-under-env-c (cnd-e3 e) env)])
               (error "NUMEX cnd applied to a non-boolean condition")))]

        [(iseq? e) 
         (let ([v1 (eval-under-env-c (iseq-e1 e) env)]
               [v2 (eval-under-env-c (iseq-e2 e) env)])
           (cond [(and (or (bool? v1) (num? v1))(or (bool? v2) (num? v2)))(cond [(and (bool? v1) (bool? v2))(cond [(equal? (bool-b v1) (bool-b v2))(bool #t)]
                                                [true (bool #f)])]
              [(and (num? v1) (num? v2))(cond [(equal? (num-int v1) (num-int v2))(bool #t)]
                                              [true (bool #f)])]
              [true (bool #f)])]
              [true (error "NUMEX equality is applied to something other than non-booleans or non-numbers")]))]


        [(ifleq? e) 
         (let ([v1 (eval-under-env-c (ifleq-e1 e) env)]
               [v2 (eval-under-env-c (ifleq-e2 e) env)])
           (if (and (num? v1) (num? v2))
               (cond [(> (num-int v1) (num-int v2))(let ([v4 (eval-under-env-c (ifleq-e4 e) env)]) v4)]
                    [(not (> (num-int v1) (num-int v2)))(let ([v3 (eval-under-env-c (ifleq-e3 e) env)]) v3)])
           (error "NUMEX ifleq condition applied to non-numbers")))]


        [(with? e)
         (let ([v1 (eval-under-env-c (with-e1 e) env)])
           (cond [(string? (with-s e)) (eval-under-env-c (with-e2 e) (cons (cons (with-s e) v1) env))]
                [(null?   (with-s e)) (eval-under-env-c (with-e2 e) (cons v1 env))]
                [true (error "NUMEX with applied to non-string")]))]

        [(apply? e)
          (let ([func (eval-under-env-c (apply-e1 e) env)]) 
            (cond 
              [(closure? func) (let ([fun-def (closure-f func)]) 
                (let (
                  [eval-e2 (eval-under-env-c (apply-e2 e) env)]
                )
                (cond 
                  [(lam? fun-def) (
                    eval-under-env-c (lam-e fun-def) 
                    (cons (cons (lam-s2 fun-def) eval-e2)
                    (cons (cons (lam-s1 fun-def) func) (closure-env func)))
                  )
                  ] 
                  [true (error "closure function isn't lam")])
                )
              )
              ]

              [(lam? func) 
              (let (
                [clos (eval-under-env-c func env)]
                ) 
                (let (
                  [lam-def (closure-f clos)]
                  [eval-e2 (eval-under-env-c (apply-e2 e) env)]
                )
                      (cond [(lam? lam-def)(eval-under-env-c (lam-e lam-def) (cons (cons (lam-s2 lam-def) eval-e2)
                                                                  (cons (cons (lam-s1 lam-def) clos) (closure-env clos))))] 
                          [true (error "closure function isn't lam")])))]
              
              [true (error (format "function type error in apply : ~v" e))]
              )
            )
          ]
        
        
        [(lam? e)
          (closure env e)]


        [(apair? e) 
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))] 

        [(1st? e) 
         (let ([v1 (eval-under-env-c (1st-e1 e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "NUMEX 1st applied to non-apair")))]

        [(2nd? e) 
         (let ([v1 (eval-under-env-c (2nd-e1 e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "NUMEX 2nd applied to non-apair")))]

        [(munit? e) (munit)] 

        [(ismunit? e)
         (let ([v1 (eval-under-env-c (ismunit-e e) env)])
           (cond[(munit? v1)(bool #t)]
                [else (bool #f)]))]

        [(closure? e) e] 

        [(letrec? e)
         (let ([e1 (letrec-e1 e)]
               [s1 (letrec-s1 e)]
               [e2 (letrec-e2 e)]
               [s2 (letrec-s2 e)]
               [e3 (letrec-e3 e)]
               [s3 (letrec-s3 e)]
               [e4 (letrec-e4 e)]
               [s4 (letrec-s4 e)]
               [e5 (letrec-e5 e)])
         (if (and (string? (letrec-s1 e)) (string? (letrec-s2 e)) (string? (letrec-s3 e)) (string? (letrec-s4 e)))
             (eval-under-env-c e5 (cons (cons s1 e1)(cons (cons s2 e2)(cons (cons s3 e3) (cons (cons s4 e4) env)))))
             (error "NUMEX letrec applied to non-string")))]


        [(key? e)
         (let ([ex (eval-under-env-c (key-e e) env)])
               (cond[(string? (key-s e)) e]
                    [else (error "key is not a string")]))]

        [(record? e)
         (let ([k (eval-under-env-c (record-k e) env)]
               [r (eval-under-env-c (record-r e) env)])
               (cond[(key? k) (cond[(or (munit? (eval-exp r)) (record? r)) (record k r)]
                                   [else (error "value of record invalid")])]
                [else (error "key invalid")]))]

        [(value? e)
          (let ([v2 (eval-under-env-c (value-r e) env)])
            (cond [(string? (value-s e)) (cond [(record? v2) (cond [(equal? (value-s e) (key-s (record-k v2))) (key-e (record-k v2))]
                                                          [(munit? (record-r v2)) (munit)]
                                                          [true (eval-under-env-c (value (value-s e) (record-r v2)) null)])]
                                      [true (error "Second value argument is not a record")])]
                  [true (error "First value argument is not a string")]))]

        [(ifnzero? e) 
         (let ([v1 (eval-under-env-c (ifnzero-e1 e) env)])
           (if (num? v1)
               (cond[(not (equal? (num-int v1) 0))(let ([v2 (eval-under-env-c (ifnzero-e2 e) env)]) v2)]
                    [(equal? (num-int v1) 0)(let ([v3 (eval-under-env-c (ifnzero-e3 e) env)]) v3)])
               (error "NUMEX ifnotzero condition applied to non-number")))]

        

        ;; CHANGE add more cases here
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
