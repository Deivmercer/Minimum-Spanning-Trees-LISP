;; Graph
(defparameter *vertices* 
    (make-hash-table :test #'equal))

(defparameter *arcs* 
    (make-hash-table :test #'equal))

(defparameter *graphs* 
    (make-hash-table :test #'equal))

(defparameter *visited* 
    (make-hash-table :test #'equal))

(defparameter *vertex-key* 
    (make-hash-table :test #'equal))

(defparameter *previous* 
    (make-hash-table :test #'equal))

;; Ritorna il graph-id stesso se questo grafo è già stato creato, oppure NILse no.  
(defun is-graph (graph-id)
    (gethash graph-id *graphs*))

;; Genera un nuovo grafo e lo inserisce nel data base (ovvero nella hash-table) dei grafi.
(defun new-graph (graph-id)
    (or (gethash graph-id *graphs*)
        (setf (gethash graph-id *graphs*) graph-id)))

;; Rimuove l’intero grafo dal sistema
(defun delete-graph (graph-id)
    (remhash graph-id *graphs*))

;; Aggiunge un nuovo vertice vertex-id al grafo graph-id.
(defun new-vertex (graph-id vertex-id)
    (setf 
        (gethash (list 'vertex graph-id vertex-id) *vertices*)
        (list 'vertex graph-id vertex-id)))

;; Ritorna una lista di vertici del grafo.
(defun graph-vertices (graph-id)
    (let ((vertices ()))
        (maphash 
            #'(lambda (&rest hash-entry) 
                    (cond 
                        ((string= graph-id (car (cdr (car hash-entry)))) (push (car hash-entry) vertices))))
            *vertices*)
        vertices))

;; Aggiunge un arco del grafo graph-id nella hash-table *arcs*.
(defun new-arc (graph-id source-vertex-id dest-vertex-id &optional (weight 0))
    (setf 
        (gethash (list 'arc graph-id source-vertex-id dest-vertex-id weight) *arcs*)
        (list 'arc graph-id source-vertex-id dest-vertex-id weight)))

;; Ritorna una lista di tutti gli archi presenti in graph-id.
(defun graph-arcs (graph-id)
    (let ((arcs ()))
        (maphash 
            #'(lambda (&rest hash-entry) 
                    (cond 
                        ((string= graph-id (car (cdr (car hash-entry)))) (push (car hash-entry) arcs))))
            *arcs*)
        arcs))

;; Ritorna una lista contenente gli archi che portano ai vertici N immediatamente raggiungibili da vertex-id.
(defun graph-vertex-neighbors (graph-id vertex-id)
    (let ((neighbors ()))
            (maphash 
                #'(lambda (&rest hash-entry) 
                        (cond 
                            ((string= graph-id (car (cdr (car hash-entry)))) 
                                (cond 
                                    ((string= vertex-id (car (cdr (cdr (car hash-entry))))) (push (car hash-entry) neighbors))
                                    ((string= vertex-id (car (cdr (cdr (cdr (car hash-entry)))))) (push (car hash-entry) neighbors))))))
                *arcs*)
            neighbors))

;; Ritorna una lista contenente i vertici adiacenti a vertex-id.
(defun graph-vertex-adjacent (graph-id vertex-id)
    (let ((adjacent ()))
            (maphash 
                #'(lambda (&rest hash-entry) 
                        (cond 
                            ((string= graph-id (car (cdr (car hash-entry)))) 
                                (cond 
                                    ((string= vertex-id (car (cdr (cdr (car hash-entry))))) 
                                        (push (gethash (list 'vertex graph-id (car (cdr (cdr (cdr (car hash-entry)))))) *vertices*) adjacent))
                                    ((string= vertex-id (car (cdr (cdr (cdr (car hash-entry)))))) 
                                        (push (gethash (list 'vertex graph-id (car (cdr (cdr (car hash-entry))))) *vertices*) adjacent))))))
                *arcs*)
            adjacent))

;; Stampa alla console dell’interprete una lista dei vertici e degli archi del grafo graph-id.
(defun graph-print (graph-id)
    (append (graph-vertices graph-id) (graph-arcs graph-id)))

;; Test
(new-graph 'grafo)(new-vertex 'grafo 'v1)(new-vertex 'grafo 'v2)(new-vertex 'grafo 'v3)(new-arc 'grafo 'v1 'v2 1)(new-arc 'grafo 'v2 'v3)