;;;; graph.lisp
;;;;
;;;; Autore: Davide Costantini
;;;;
;;;; Libreria per la gestione di grafi diretti, connessi e con cicli.

;;; Tabella contenente le rappresentazioni dei grafi.
(defparameter *graphs* (make-hash-table :test #'equal))

;;; Tabella contenente le rappresentazioni dei grafi.
(defparameter *vertices* (make-hash-table :test #'equal))

;;; Tabella contenente le rappresentazioni degli archi.
(defparameter *arcs* (make-hash-table :test #'equal))

(defun is-graph (graph-id)
  "Ritorna graph-id se esiste un grafo con questo nome, NIL altrimenti."
  (cond ((gethash graph-id *graphs*) graph-id)))

(defun new-graph (graph-id)
  "Crea, se non esiste al momento della chiamata, un grafo chiamato graph-id e
ritorna graph-id."
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))

(defun new-vertex (graph-id vertex-id)
  "Crea un nuovo vertice chiamato vertex-id relativo al grafo graph-id e ritorna
la rappresentazione del vertice. Se il grafo graph-id non esiste al momento 
della chiamata allora verrà creato."
  (cond ((not (is-graph graph-id))
         (new-graph graph-id)))
  (setf 
   (gethash (list graph-id vertex-id) *vertices*) 
   (list 'vertex graph-id vertex-id)))

(defun graph-vertices (graph-id)
  "Ritorna una lista contenente tutti i vertici relativi al grafo graph-id."
  (cond ((not (is-graph graph-id))
         (error "Il grafo specificato non esiste.")))
  (let ((vertices ()))
    (maphash 
     #'(lambda (key value) 
         (cond ((string= graph-id (car key))
                (push value vertices))))
     *vertices*)
    vertices))

(defun new-arc (graph-id source-vertex-id dest-vertex-id &optional (weight 0))
  "Crea un nuovo arco per il grafo graph-id che va dal vertice source-vertex-id
al vertice dest-vertex-id con peso weight e ritorna la rappresentazione
dell'arco. Se i due vertici non esistono al momento della chiamata allora
verranno creati."
  (cond ((not (gethash (list graph-id source-vertex-id) *vertices*))
         (new-vertex graph-id source-vertex-id)))
  (cond ((not (gethash (list graph-id dest-vertex-id) *vertices*))
         (new-vertex graph-id dest-vertex-id)))
  (cond
   ((gethash (list graph-id source-vertex-id dest-vertex-id) *arcs*)
    (remhash (list graph-id source-vertex-id dest-vertex-id) *arcs*))
   ((gethash (list graph-id dest-vertex-id source-vertex-id) *arcs*)
    (remhash (list graph-id dest-vertex-id source-vertex-id) *arcs*)))
  (setf 
   (gethash (list graph-id source-vertex-id dest-vertex-id) *arcs*)
   (list 'arc graph-id source-vertex-id dest-vertex-id weight)))

(defun graph-arcs (graph-id)
  "Ritorna una lista contenente tutti gli archi relativi al grafo graph-id."
  (cond ((not (is-graph graph-id))
         (error "Il grafo specificato non esiste.")))
  (let ((arcs ()))
    (maphash 
     #'(lambda (key value) 
         (cond ((string= graph-id (car key))
                (push value arcs))))
     *arcs*)
    arcs))

(defun graph-vertex-neighbors (graph-id vertex-id)
  "Ritorna una lista contenente gli archi che portano ai vertici adiacenti a
vertex-id."
  (cond 
   ((not (is-graph graph-id))
    (error "Il grafo specificato non esiste."))
   ((not (gethash (list graph-id vertex-id) *vertices*))
    (error "Il vertice specificato non esiste.")))
  (let ((neighbors ()))
    (maphash
     #'(lambda (key value) 
         (cond ((string= graph-id (car key))
                (cond 
                 ((numberp vertex-id)
                  (cond
                   ((= vertex-id (car (cdr key))) 
                    (push value neighbors))
                   ((= vertex-id (car (cdr (cdr key)))) 
                    (push value neighbors))))
                 (t (cond
                     ((string= vertex-id (car (cdr key))) 
                      (push value neighbors))
                     ((string= vertex-id (car (cdr (cdr key)))) 
                      (push value neighbors))))))))
     *arcs*)
    neighbors))

(defun graph-vertex-adjacent (graph-id vertex-id)
  "Ritorna una lista contenente i vertici adiacenti a vertex-id."
  (cond 
   ((not (is-graph graph-id))
    (error "Il grafo specificato non esiste."))
   ((not (gethash (list graph-id vertex-id) *vertices*))
    (error "Il vertice specificato non esiste.")))
  (let ((adjacent ()))
    (maphash 
     #'(lambda (&rest hash-entry) 
         (cond ((string= graph-id (car (car hash-entry)))
                (cond 
                 ((numberp vertex-id)
                  (cond 
                   ((= vertex-id (car (cdr (car hash-entry))))
                    (push 
                     (gethash 
                      (list graph-id (car (cdr (cdr (car hash-entry)))))
                      *vertices*) 
                     adjacent))
                   ((= vertex-id (car (cdr (cdr (car hash-entry)))))
                    (push 
                     (gethash 
                      (list graph-id (car (cdr (car hash-entry))))
                      *vertices*) 
                     adjacent))))
                 (t (cond 
                     ((string= vertex-id (car (cdr (car hash-entry))))
                      (push 
                       (gethash 
                        (list graph-id (car (cdr (cdr (car hash-entry)))))
                        *vertices*) 
                       adjacent))
                     ((string= vertex-id (car (cdr (cdr (car hash-entry)))))
                      (push 
                       (gethash 
                        (list graph-id (car (cdr (car hash-entry))))
                        *vertices*) 
                       adjacent))))))))
     *arcs*)
    adjacent))

(defun graph-print (graph-id)
  "Ritorna una lista contenente i vertici e gli archi relativi al grafo
graph-id"
  (append (graph-vertices graph-id) (graph-arcs graph-id)))

(defun graph-find-arc (graph-id source-vertex-id dest-vertex-id)
  "Ritorna l'arco che collega source-vertex-id a dest-vertex-id."
  (or (gethash (list graph-id source-vertex-id dest-vertex-id) *arcs*)
      (gethash (list graph-id dest-vertex-id source-vertex-id) *arcs*)))