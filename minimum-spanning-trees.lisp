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
                        ((string= graph-id (car (cdr (car hash-entry)))) 
                            (push (car hash-entry) vertices))))
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
                        ((string= graph-id (car (cdr (car hash-entry)))) 
                            (push (car hash-entry) arcs))))
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
                                    ((string= vertex-id (car (cdr (cdr (car hash-entry))))) 
                                        (push (car hash-entry) neighbors))
                                    ((string= vertex-id (car (cdr (cdr (cdr (car hash-entry)))))) 
                                        (push (car hash-entry) neighbors))))))
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
                                        (push 
                                            (gethash 
                                                (list 'vertex graph-id (car (cdr (cdr (cdr (car hash-entry)))))) 
                                                *vertices*) 
                                            adjacent))
                                    ((string= vertex-id (car (cdr (cdr (cdr (car hash-entry)))))) 
                                        (push 
                                            (gethash 
                                                (list 'vertex graph-id (car (cdr (cdr (car hash-entry))))) 
                                                *vertices*) 
                                            adjacent))))))
                *arcs*)
            adjacent))

;; Stampa alla console dell’interprete una lista dei vertici e degli archi del grafo graph-id.
(defun graph-print (graph-id)
    (append (graph-vertices graph-id) (graph-arcs graph-id)))

;; Test
(new-graph 'grafo)
(new-vertex 'grafo 'v1)
(new-vertex 'grafo 'v2)
(new-vertex 'grafo 'v3)
(new-arc 'grafo 'v1 'v2 1)
(new-arc 'grafo 'v2 'v3)

;; Heap
(defparameter *heaps* 
    (make-hash-table :test #'equal))

;; Inserisce un nuovo heap nella hash-table *heaps*
(defun new-heap (heap-id &optional (capacity 42))
    (or (gethash heap-id *heaps*)
        (setf 
            (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity)))))

;; Rimuove tutto lo heap indicizzato da heap-id.
(defun heap-delete (heap-id)
    (remhash heap-id *heaps*))

;; Ritorna la dimensione dello heap
(defun heap-size (heap-id)
    (car (cdr (cdr (gethash heap-id *heaps*)))))

;; Ritorna l'effettiva implementazione dello heap
(defun heap-actual-heap (heap-id)
    (car (cdr (cdr (cdr (gethash heap-id *heaps*))))))

;; Questo predicato è vero quando lo heap heap-id non contiene elementi.
(defun heap-empty (heap-id)
    (cond 
        ((gethash heap-id *heaps*)
            (cond
                ((= (heap-size heap-id) 0))))
        (t (error "Lo heap specificato non esiste."))))

;; Questo predicato è vero quando lo heap heap-id contiene almeno un elemento.
(defun heap-not-empty (heap-id)
    (cond 
        ((gethash heap-id *heaps*)
            (cond
                ((= (heap-size heap-id) 0) nil)
                (t t)))
        (t (error "Lo heap specificato non esiste."))))

;; Ritorna una lista di due elementi dove K è la chiave minima e V il valore associato.
(defun heap-head (heap-id)
    (cond 
        ((gethash heap-id *heaps*)
            (cond
                ((heap-not-empty heap-id) (aref (heap-actual-heap heap-id) 0))
                (t (error "Lo heap specificato e' vuoto."))))
        (t (error "Lo heap specificato non esiste."))))

;; La funzione heap-insert inserisce l’elemento V nello heap heap-id con chiave K.
(defun heapify-insert (heap-id index)
    (cond
        ((= index 0) t)
        (t (let ((current (aref (heap-actual-heap heap-id) index))
                (parent (aref (heap-actual-heap heap-id) (floor (/ index 2)))))
                (cond
                    ((< (car current) (car parent)) 
                        (setf (aref (heap-actual-heap heap-id) index) parent)
                        (setf (aref (heap-actual-heap heap-id) (floor (/ index 2))) current)
                        (heapify-insert heap-id (floor (/ index 2))))))
            t)))
 
(defun heap-insert (heap-id K V)
    (cond 
        ((not (gethash heap-id *heaps*))
            (new-heap heap-id)))
    (setf 
        (aref 
            (heap-actual-heap heap-id)
            (heap-size heap-id))
        (list K V))
    (setf
        (gethash heap-id *heaps*)
        (list 'heap heap-id 
            (+ (heap-size heap-id) 1)
            (heap-actual-heap heap-id)))
    (heapify-insert heap-id (- (heap-size heap-id) 1)))

;; Ritorna la lista K, V con K minima e rimuove la coppia dallo heap
(defun smallest (heap index-x index-y size)
    (cond 
        ((< index-x size)
            (cond
                ((<= (car (aref heap index-x)) (car (aref heap index-y)))
                    index-x)
                (t index-y)))
        (t index-y)))

(defun swap (heap index to-swap)
    (cond
        ((/= index to-swap)
            (let ((x (aref heap index))
                (y (aref heap to-swap)))
                (setf (aref heap index) y)
                (setf (aref heap to-swap) x))
            to-swap)
        (t -1)))

(defun heapify (heap index size)
    (cond
        ((/= index -1) 
            (heapify heap 
                (swap heap index (smallest heap (- (+ (* (+ index 1) 2) 1) 1)
                    (smallest heap (- (* (+ index 1) 2) 1) index size)
                    size))
                size))))

(defun heap-extract (heap-id)
    (let ((current-head (heap-head heap-id))
        (heap (heap-actual-heap heap-id))
        (size (heap-size heap-id)))
        (setf 
            (aref heap 0) 
            (aref heap (- size 1)))
        (setf
            (aref heap (- size 1))
            0)
        (setf
            (gethash heap-id *heaps*)
            (list 'heap heap-id (- size 1) heap))
        (cond
            ((> size 0) 
                (heapify heap 0 (- size 1))))
        current-head))

(defun mapheap (func heap heap-size &optional (index 1) )
    (cond
        ((< (- index 1) heap-size)
            (funcall func (aref heap (- index 1)) (- index 1))
            (mapheap func heap heap-size (* index 2))
            (mapheap func heap heap-size (+ (* index 2) 1)))))

;; Sostituisce la chiave OldKey (associata al valore V) con NewKey. 
(defun heap-modify-key (heap-id new-key old-key V)
    (cond
        ((not (gethash heap-id *heaps*))
            (error "Lo heap specificato non esiste"))
        ((= new-key old-key) t)
        (t (mapheap 
            #'(lambda (heap-entry index) 
                (cond
                    ((= (car heap-entry) old-key)
                        (cond
                            ((= (car (cdr heap-entry)) V)
                                (setf
                                    (aref (heap-actual-heap heap-id) index)
                                    (list new-key V))
                                (cond
                                    ((< new-key old-key)
                                        (heapify-insert heap-id index))
                                    ((> new-key old-key)
                                        (heapify (heap-actual-heap heap-id)
                                            index (heap-size heap-id)))))))))
            (heap-actual-heap heap-id)
            (heap-size heap-id)))))

;; Stampa sulla console lo stato interno dello heap heap-id.
(defun heap-print (heap-id)
    (cond 
        ((gethash heap-id *heaps*)
            (let ((heap-entries ()))
                (mapheap 
                    #'(lambda (heap-entry index) 
                        (push (cons index heap-entry) heap-entries))
                    (heap-actual-heap heap-id)
                    (heap-size heap-id))
                (print (reverse heap-entries)))
                t)
        (t (error "Lo heap specificato non esiste."))))

;; Test
(heap-insert 'heap 3 3)
(heap-insert 'heap 2 2)
(heap-insert 'heap 1 1)
(heap-insert 'heap 4 4)