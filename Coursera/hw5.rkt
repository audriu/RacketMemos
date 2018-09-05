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

(define (racketlist->mupllist l)
  (if (null? l)
      (aunit)
      (apair (car l)(racketlist->mupllist (cdr l)))))

(define (mupllist->racketlist l)
  (if (aunit? l)
      null
      (cons (apair-e1 l)(mupllist->racketlist (apair-e2 l)))))

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
        [(int? e)e]
        [(snd? e) (apair-e2 (snd-e e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (int-num (ifgreater-e1 e))]
               [v2 (int-num (ifgreater-e2 e))])
           (if (> v1 v2)
               (ifgreater-e3 e)
               (ifgreater-e4 e)))]
        [(mlet? e)
         (let* ([var (mlet-var e)]
               [e-new (mlet-e e)]
               [body (mlet-body e)]
               [new-env (cons (cons var e-new) env)])
           (eval-under-env body new-env))]
        [(call? e)
         (println e)
         (let* ([clos (call-funexp e)]
                [_0 (println clos)]
                
                [arg (call-actual e)]
                [_1(println arg)]
                
                [env-l (closure-env clos)]
                [_2(println env-l)]
                
                [fun (closure-fun clos)]
                [_3(println fun)]
                
                [param-name (fun-formal fun)]
                [_4(println param-name)]
                
                [new-env (cons (cons param-name arg) env-l)]
                [_5(println new-env)]
                
                [body (fun-body fun)]
                [_6(println body)]

                )
           (eval-under-env body new-env))]
        [(isaunit? e)
         (let ([e (call (isaunit-e e) env)])
           (if (aunit? e)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (isaunit? (eval-exp (isaunit e1)))
      e2
      e3))

(define (mlet* lstlst e2)
  (eval-under-env e2 lstlst))

(define (ifeq e1 e2 e3 e4)
  (if (= (int-num(eval-exp e1))(int-num(eval-exp e2)))
      e3
      e4))

;; Problem 4
 ;(eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))
(define mupl-map 7)

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

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
