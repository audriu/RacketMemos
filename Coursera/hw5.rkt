;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (apair (car lst)(racketlist->mupllist (cdr lst)))))

(define (mupllist->racketlist mlst)
  (if (aunit? mlst)
      null
      (cons (apair-e1 mlst)(mupllist->racketlist (apair-e2 mlst)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) e]
        [(aunit? e)e]
        [(closure? e)
         (closure env (closure-fun e))]
        [(fun? e) e]
        [(call? e)
         (let ([cloj (eval-under-env (call-funexp e) env)]
               [argument (eval-under-env (call-actual e) env)])
           (if (not (closure? cloj))
               (error "not closure")
               (let* ([env-c (closure-env cloj)]
                      [fun (eval-under-env (closure-fun cloj) env-c)]
                      [nameopt (fun-nameopt fun)]
                      [formal (fun-formal fun)]
                      [body (fun-body fun)]
                      [new-env (if nameopt (cons (cons nameopt cloj) env-c) env-c)]
                      [new-env- (cons (cons formal argument) new-env)])
                 (eval-under-env body new-env-))))]
        [(add? e)
         (display "add")
         (display e)
         (display env)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (> (int-num v1) (int-num v2))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env)))]
        [(mlet? e)
         (let* ([var (mlet-var e)]
                [e-new (mlet-e e)]
                [body (mlet-body e)]
                [new-env (cons (cons var e-new) env)])
           (eval-under-env body new-env))]
        [(apair? e)
         (let ([pirm (eval-under-env (apair-e1 e) env)]
               [antr (eval-under-env (apair-e2 e) env)])
           (apair pirm antr))]
        [(snd? e)
         (apair-e2 (eval-under-env (snd-e e) env))]
        [(fst? e)
         (apair-e1 (eval-under-env (fst-e e) env))]
        [(isaunit? e)
         (let ([res (eval-under-env (isaunit-e e) env)])
           (if(aunit? res)
              (int 1)
              (int 0)))]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (let ([res (eval-exp e1)])
    (if (aunit? res)
        (eval-exp e2)
        (eval-exp e3))))

(define (mlet* lstlst e2)
  (define (eval-env-list list- acc)
    (if (null? list-)
        acc
        (eval-env-list (cdr list-)
                       (cons
                        (cons
                         (car (car list-))
                         (eval-under-env (cdr (car list-)) acc))
                        acc))))
  (eval-under-env e2 (eval-env-list lstlst null)))

(define (ifeq e1 e2 e3 e4)
  (let ([e1- (eval-exp e1)]
        [e2- (eval-exp e2)])
    (if (eq? e1- e2-)
        (eval-exp e3)
        (eval-exp e4))))

;; Problem 4

(define mupl-map
  (closure '() (fun #f "fun1" (closure '() (fun "rep" "lst" (ifgreater (isaunit (var "lst")) (int 0)
                                                                       (aunit)
                                                                       (apair (call (closure '() (var "fun1")) (fst(var "lst")))
                                                                              (call (var "rep") (snd (var "lst"))))))))))

(define mupl-mapAddN null)

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
