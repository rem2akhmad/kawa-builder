;;;
;;; Билдер для kawa компилятора
;;;
;;
;; @created   "Wed Jan 02 16:04:51 MSK 2008"
;;
(module-static #t)
(require 'list-lib)
;;;;;;; определение ориентированного графа модулей ;;;;;;;;;;;;;;;

(define (make-edge nodefrom nodeto)
  (cons nodefrom nodeto))

(define (get-node-from edge)
  (car edge))

(define (get-node-to edge)
  (cdr edge))

(define (make-graph nodes edges)
  (cons nodes edges))

(define (add-node node nodes)
  (cons node nodes))

(define (add-edge edge edges)
  (cons edge edges))

(define (add-edges edges1 edges)
  (fold add-edge edges edges1))

(define (get-nodes graph)
  (car graph))

(define (get-edges graph)
  (cdr graph))

(define (remove-node graph node)
  (let ((nodes (get-nodes graph))
        (edges (get-edges graph)))
    (make-graph (remove (lambda (x) (eq? x node)) nodes) 
                (remove (lambda (x) (eq? (get-node-from x) node)) edges))))

(define (remove-nodes graph nodes)
  (if (not (null? nodes))
      (remove-nodes (remove-node graph (car nodes))
                    (cdr nodes))
      graph))

;;;;;;;;;;;;;;;;; топологическая сортировка ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; поиск вершин не имеющих входящих дуг
(define (nodes-no-incoming graph)
  (let ((nodes (get-nodes graph)))
    (filter (lambda (x)
              (null? (nodes-out-to x graph)))
            nodes)))

;; составление множества вершин, имеющих исходящие дуги в данную вешину
(define (nodes-out-to node graph)
  (let ((edges (get-edges graph)))
    (map get-node-from
         (filter (lambda (x)
                   (eq? node (get-node-to x)))
                 edges))))

;; собственно сортировка
(define (topological-sort graph)
  (let ((nodes (nodes-no-incoming graph)))
    (if (not (null? nodes))
        (append nodes 
                (topological-sort (remove-nodes graph nodes)))
        '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; чтение модулей и составление графа ;;;;;;;;;;;
;; возвращает список модулей, которые требуются данному модулю
;; fname - имя файла, содержащий модуль
(define (required-modules fname)
  (define (reader f modules)
    (let ((exp (read f)))
      (cond ((eof-object? exp) modules)
            ((require? exp) (reader f (cons (required-module exp) modules)))
            (else
             (reader f modules)))))
  (let ((mfile (open-input-file fname)))
    (reader mfile '())))

;; возвращает префикс модуля
(define (module-prefix md)
  (let ((str (symbol->string md)))
    (let ((mod-str (substring str 1 (- (string-length str) 1))))
      (substring
       mod-str
       0
       (string-last-of mod-str #\.)))))

;; преобразует название модуля в имя файла
(define (modulename->filename md)
  (let ((str (symbol->string md)))
    (let ((mod-str (substring str 1 (- (string-length str) 1))))
      (string-append 
       (list->string
        (map (lambda (x)
               (if (char-ci=? x #\.)
                   #\/
                   x))
             (string->list mod-str)))
       ".scm"))))

;; предикат, определяет является ли данное выражение выражением require
(define (require? exp)
  (and (eq? (car exp) 'require)
       (symbol? (required-module exp))))

;; возвращает название модуля из require
(define (required-module ex)
  (cadr ex))

;; возвращает список файлов в директории
(define (dir directory)
  (array->list (*:list
                (<java.io.File>
                 (*:toString directory)))))

;; array to list
(define (array->list arr)
  (define (iter-conv lst i)
    (if (= i arr:length)
        lst
        (iter-conv (cons (arr i) lst) (+ i 1))))
  (map symbol->string (iter-conv '() 0)))

;; рекурсивный обход дерева подкатологов
(define (dir-tree parent)
  (define (dir-it files p)
    (cond ((null? files) '())
          ((file-directory? (string-append p (car files) "/")) (append
                                                                 (dir-it (dir (string-append p (car files) "/")) (string-append p (car files) "/"))
                                                                 (dir-it (cdr files) p)))
          (else (cons (string-append p (car files))
                      (dir-it (cdr files) p)))))
  (dir-it (dir parent) parent))



;; имя модуля из файла
(define (get-module-name filen prefix)
  (define (substitute-slash str c)
    (list->string
     (map (lambda (x)
            (if (or (char=? x #\/) (char=? x #\\))
                c
                x))
          (string->list str))))
  (let ((sfn (substring filen (string-length prefix) (string-length filen))))
    (string->symbol
     (string-append "<"
                    (substitute-slash (substring sfn 0 (- (string-length sfn) 4)) #\.)
                    ">"))))

;; строит граф модулей
(define (make-modules-graph filelist src-dir)
  ;; список дуг
  (define (make-edges node nodes)
    (map (lambda (x)
           (make-edge node x))
         nodes))
  
  (let ((graph (make-graph '() '())))
    (let ((nodes (get-nodes graph))
          (edges (get-edges graph)))
      (make-graph (fold (lambda (fn fns)
                          (add-node (get-module-name fn src-dir) fns))
                        nodes
                        filelist)
                  (fold (lambda (fn fns)
                          (let ((nod (get-module-name fn src-dir))
                                (reqmods (required-modules fn)))
                            (add-edges
                             (add-edges (make-edges nod reqmods) edges)
                             fns)))
                        edges
                        filelist)))))

;; граф модулей по директории с исходниками
(define (modules-graph-from-dir src-dir)
  (make-modules-graph
   (filter (lambda (x)
            (string=? (path-extension x) "scm"))
          (dir-tree src-dir))
   src-dir))
;;;;;;;;;;;;;;;;;;;;;;;;;;;; компиляция отсортированных модулей ;;;;;;;;;
(define (compile-modules modules-list out-dir src-dir)
  (for-each 
   (lambda (mod)
     (system
      (string-append "cmd /C java -cp \"" out-dir ";%CLASSPATH%\" kawa.repl -d " out-dir " -P "
                     (module-prefix mod) "."
                     " -C " src-dir
                     (modulename->filename mod)
                     " 2>>" out-dir "errors"
                     )))
   modules-list))

(define (main out-dir src-dir)
  (compile-modules
   (reverse! (topological-sort (modules-graph-from-dir src-dir)))
   out-dir
   src-dir))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; строковые операции ;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO надо сделать с учетом поиска отсутствующего символа
(define (string-index-of str chr)
  (list-index 
   (lambda (x)
     (char=? chr x))
   (string->list str)))

(define (string-last-of str chr)
  (- (string-length str) 1
     (list-index 
      (lambda (x)
        (char=? chr x))
      (reverse! (string->list str)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test block ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (build-graph1)
  (let ((nodes '())
        (edges '()))
    (begin
      ;; list of nodes
      (set! nodes (add-node 'a nodes))
      (set! nodes (add-node 'b nodes))
      (set! nodes (add-node 'c nodes))
      (set! nodes (add-node 'd nodes))
      (set! nodes (add-node 'e nodes))
      ;; list of edges
      (set! edges (add-edge (make-edge 'a 'c) edges))
      (set! edges (add-edge (make-edge 'a 'b) edges))
      (set! edges (add-edge (make-edge 'a 'd) edges))
      (set! edges (add-edge (make-edge 'b 'd) edges))
      (set! edges (add-edge (make-edge 'c 'd) edges))
      (set! edges (add-edge (make-edge 'a 'e) edges))
      (set! edges (add-edge (make-edge 'c 'e) edges))
      (set! edges (add-edge (make-edge 'd 'e) edges))
      (make-graph nodes edges))))

(define (test-remove-node)
  (let ((graph (build-graph1)))
    (remove-node graph 'a)))

(define (test-module-graph)
  (make-modules-graph
   (filter (lambda (x) 
             (string=? (path-extension x) "scm")) 
           (dir-tree "../src/"))
   "../src/"))

(define (test-sort graph)
  (topological-sort (test-module-graph)))
(define (test-comp)
  (compile-modules 
   (reverse! (topological-sort (modules-graph-from-dir "../src/"))) 
   "./" 
   "../src/"))
(if (< (vector-length command-line-arguments) 2)
    (display "set out-dir and src-dir")
    (main (vector-ref command-line-arguments 0) (vector-ref command-line-arguments 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;main;;;;;;;;;;;;;;;;;;;;
