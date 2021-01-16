;;;; mst.lisp
;;;;
;;;; Autore: Davide Costantini
;;;;
;;;; Libreria per il calcolo del Minimum Spanning Tree di un grafo.

(load "graph.lisp")
(load "heap.lisp")

;;; Tabella contenente le rappresentazioni degli elementi dell'MST.
(defparameter *vertex-key* (make-hash-table :test #'equal))

;;; Tabella contenente la rappresentazione delle precedenze degli elementi
;;; dell'MST.
(defparameter *previous* (make-hash-table :test #'equal))

;;; Tabella contenente i vertici visitati durante l'esecuzione dell'algoritmo.
(defparameter *visited* (make-hash-table :test #'equal))

(defun mst-vertex-key (graph-id vertex-id)
  "Ritorna il peso dell'arco del grafo graph-id che connette il vertice 
vertex-id nell'MST."
  (or (gethash (list graph-id vertex-id) *vertex-key*)
      most-positive-double-float))

(defun mst-previous (graph-id V)
  "Ritorna il vertice genitore del vertice V nell'MST."
  (or (gethash (list graph-id V) *previous*)
      nil))

(defun delete-mst (graph-id)
  "Rimuove le componenti dell'MST dalle rispettive hash table e ritorna NIL."
  (remhash graph-id *heaps*)
  (maphash (lambda (&rest hash-entry) 
             (cond ((string= graph-id (car (car hash-entry)))
                    (remhash (car hash-entry) *vertex-key*))))
           *vertex-key*)
  (maphash (lambda (&rest hash-entry) 
             (cond ((string= graph-id (car (car hash-entry)))
                    (remhash (car hash-entry) *previous*))))
           *previous*)
  (maphash (lambda (&rest hash-entry) 
             (cond ((string= graph-id (car (car hash-entry)))
                    (remhash (car hash-entry) *visited*))))
           *visited*))

(defun mst-update-node (graph-id vertex-id parent-id weight)
  "Aggiorna il peso dell'arco del grafo graph-id che connette il vertice 
vertex-id all'interno dell'MST se weight è più piccolo del peso attuale e
ritorna T."
  (cond ((not (gethash (list graph-id vertex-id) *visited*))
         (let ((old-key (mst-vertex-key graph-id vertex-id)))
           (cond ((< weight (mst-vertex-key graph-id vertex-id))
                  (setf (gethash (list graph-id vertex-id) *vertex-key*) 
                        weight)
                  (setf (gethash (list graph-id vertex-id) *previous*) 
                        parent-id)
                  (heap-modify-key graph-id weight old-key 
                                   vertex-id)))))))

(defun mst-build-tree (graph-id)
  "Costruisce l'MST cercando l'arco col peso minimo che connette ogni vertice al
suo nodo genitore."
  (cond ((heap-not-empty graph-id)
         (let ((head (car (cdr (heap-extract graph-id)))))
           (setf (gethash (list graph-id head) *visited*) (list graph-id head))
           (mapc (lambda (arc)
                   (mst-update-node graph-id (arc-destination arc) head 
                                    (arc-weight arc)))
                 (graph-vertex-neighbors graph-id head))
           (mst-build-tree graph-id)))))

(defun mst-prim (graph-id source)
  "Costruisce MST applicando l'algoritmo di Prim al grafo graph-id partendo dal
vertice source e ritorna NIL."
  (cond ((not (is-graph graph-id))
         (error "Il grafo specificato non esiste."))
        ((not (gethash (list graph-id source) *vertices*))
         (error "Il vertice specificato non esiste.")))
  (delete-mst graph-id)
  (new-heap graph-id (length (graph-vertices graph-id)))
  (mapc (lambda (vertex)
          (heap-insert graph-id most-positive-double-float 
                       (car (cdr (cdr vertex))))) 
        (graph-vertices graph-id))
  (heap-modify-key graph-id 0 most-positive-double-float source)
  (setf (gethash (list graph-id source) *vertex-key*) 
        most-positive-double-float)
  (setf (gethash (list graph-id source) *previous*) nil)
  (mst-build-tree graph-id))

(defun mst-get-childs (graph-id node)
  "Cerca i nodi figli dell'elemento node all'interno dell'MST graph-id."
  (let ((childs ()))
    (maphash (lambda (key value) 
               (cond ((not (null value))
                      (cond ((string= graph-id (car key))
                             (cond ((numberp node)
                                    (cond ((= node value) 
                                           (push (list (car (cdr key)) 
                                                       (gethash 
                                                        (list graph-id 
                                                              (car (cdr key))) 
                                                        *vertex-key*))
                                                 childs))))
                                   (t (cond ((string= node value) 
                                             (push (list (car (cdr key)) 
                                                         (gethash 
                                                          (list graph-id 
                                                                (car (cdr key))) 
                                                          *vertex-key*))
                                                   childs))))))))))
             *previous*)
    childs))

(defun mst-sort-childs (x y)
  "Ritorna T se il primo elemento della lista x è maggiore del primo elemento
della lista y; se i primi elementi delle rispettive liste sono uguali
ritorna T se i secondi elementi delle rispettive liste sono numeri e 
l'elemento di x è maggiore di quello di y; se i secondi elementi non sono
numeri ritorna T se il secondo elemento di x è lessicograficamente più 
grande dell'elemento di y; Altrimenti, se nessuna di queste condizioni è
soddisfatta, ritorna NIL."
  (cond ((> (car (cdr x)) (car (cdr y))) t)
        ((= (car (cdr x)) (car (cdr y)))
         (cond ((numberp (car x))
                (cond ((> (car x) (car y)) t)))
               (t (cond ((string> (car x) (car y)) t)))))))

(defun mst-preorder-tree (graph-id source childs preorder-tree)
  "Effettua la visita in preordine degli elementi dell'MST graph-id e ritorna
la lista che li contiene."
  (cond ((not (null (car childs)))
         (let ((sub-tree (mst-preorder-tree graph-id (car (car childs))
                                            (sort (mst-get-childs graph-id       
                                                                  (car 
                                                                   (car
                                                                    childs))) 
                                                  'mst-sort-childs)
                                            preorder-tree)))
           (push (list graph-id source (car (car childs))
                       (gethash (list graph-id (car (car childs))) 
                                *vertex-key*)) 
                 sub-tree)
           (cond ((not (null (cdr childs)))
                  (mst-preorder-tree graph-id source (cdr childs) sub-tree))
                 (t sub-tree))))
        (t preorder-tree)))

(defun mst-get (graph-id source)
  "Ritorna la lista contenente gli elementi dell'MST graph-id visitati in
preordine."
  (cond ((not (is-graph graph-id))
         (error "Il grafo specificato non esiste."))
        ((not (null (gethash (list graph-id source) *previous*)))
         (error "Il vertice specificato non e' la radice dell'MST")))
  (mst-preorder-tree graph-id source 
                     (sort (mst-get-childs graph-id source) 
                           'mst-sort-childs) ()))

(defun delete-graph (graph-id)
  "Rimuove tutto il grafo (compresi archi, vertici ed MST) dalle rispettive hash 
table e ritorna NIL."
  (remhash graph-id *graphs*)
  (maphash (lambda (&rest hash-entry) 
             (cond ((string= graph-id (car (car hash-entry)))
                    (remhash (car hash-entry) *vertices*))))
           *vertices*)
  (maphash (lambda (&rest hash-entry) 
             (cond ((string= graph-id (car (car hash-entry)))
                    (remhash (car hash-entry) *arcs*))))
           *arcs*)
  (delete-mst graph-id))
