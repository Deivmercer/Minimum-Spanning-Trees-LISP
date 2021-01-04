;; Graph
(defparameter *vertices* 
    (make-hash-table :test #'equal))

(defparameter *arcs* 
    (make-hash-table :test #'equal))

(defparameter *graphs* 
    (make-hash-table :test #'equal))

;; Ritorna il graph-id stesso se questo grafo è già stato creato, oppure NIL se no.  
(defun is-graph (graph-id)
    (cond
        ((gethash graph-id *graphs*)
            graph-id)))

;; Genera un nuovo grafo e lo inserisce nel data base (ovvero nella hash-table) dei grafi.
(defun new-graph (graph-id)
    (or (gethash graph-id *graphs*)
        (setf (gethash graph-id *graphs*) graph-id)))

;; Rimuove l’intero grafo dal sistema
(defun delete-graph (graph-id)
    (remhash graph-id *graphs*)
    (maphash 
        #'(lambda (&rest hash-entry) 
            (cond 
                ((string= graph-id (car (car hash-entry)))
                    (remhash (car hash-entry) *vertices*))))
        *vertices*)
    (maphash 
        #'(lambda (&rest hash-entry) 
            (cond 
                ((string= graph-id (car (car hash-entry)))
                    (remhash (car hash-entry) *arcs*))))
        *arcs*))

;; Aggiunge un nuovo vertice vertex-id al grafo graph-id.
(defun new-vertex (graph-id vertex-id)
    (cond 
        ((not (is-graph graph-id))
            (new-graph graph-id)))
    (setf 
        (gethash (list graph-id vertex-id) *vertices*)
        (list 'vertex graph-id vertex-id)))

;; Ritorna una lista di vertici del grafo.
(defun graph-vertices (graph-id)
    (cond 
        ((not (is-graph graph-id))
            (error "Il grafo specificato non esiste.")))
    (let ((vertices ()))
        (maphash 
            #'(lambda (key value) 
                (cond 
                    ((string= graph-id (car key))
                        (push value vertices))))
            *vertices*)
        vertices))

;; Aggiunge un arco del grafo graph-id nella hash-table *arcs*.
(defun new-arc (graph-id source-vertex-id dest-vertex-id &optional (weight 0))
    (cond 
        ((not (gethash (list graph-id source-vertex-id) *vertices*))
            (new-vertex graph-id source-vertex-id)))
    (cond 
        ((not (gethash (list graph-id dest-vertex-id) *vertices*))
            (new-vertex graph-id dest-vertex-id)))
    (cond
        ((gethash (list graph-id source-vertex-id dest-vertex-id) *arcs*)
            (remhash (list graph-id source-vertex-id dest-vertex-id) *arcs*))
        ((gethash (list graph-id dest-vertex-id source-vertex-id) *arcs*)
            (remhash (list graph-id dest-vertex-id source-vertex-id) *arcs*)))
    (setf 
        (gethash (list graph-id source-vertex-id dest-vertex-id) *arcs*)
        (list 'arc graph-id source-vertex-id dest-vertex-id weight)))

;; Ritorna una lista di tutti gli archi presenti in graph-id.
(defun graph-arcs (graph-id)
    (cond 
        ((not (is-graph graph-id))
            (error "Il grafo specificato non esiste.")))
    (let ((arcs ()))
        (maphash 
            #'(lambda (key value) 
                (cond 
                    ((string= graph-id (car key))
                        (push value arcs))))
            *arcs*)
        arcs))

;; Ritorna una lista contenente gli archi che portano ai vertici N immediatamente raggiungibili da vertex-id.
(defun graph-vertex-neighbors (graph-id vertex-id)
    (cond 
        ((not (is-graph graph-id))
            (error "Il grafo specificato non esiste."))
        ((not (gethash (list graph-id vertex-id) *vertices*))
            (error "Il vertice specificato non esiste.")))
    (let ((neighbors ()))
        (maphash 
            #'(lambda (key value) 
                (cond 
                    ((string= graph-id (car key)) 
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

;; Ritorna una lista contenente i vertici adiacenti a vertex-id.
(defun graph-vertex-adjacent (graph-id vertex-id)
    (cond 
        ((not (is-graph graph-id))
            (error "Il grafo specificato non esiste."))
        ((not (gethash (list graph-id vertex-id) *vertices*))
            (error "Il vertice specificato non esiste.")))
    (let ((adjacent ()))
        (maphash 
            #'(lambda (&rest hash-entry) 
                (cond 
                    ((string= graph-id (car (car hash-entry)))
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

;; Stampa alla console dell’interprete una lista dei vertici e degli archi del grafo graph-id.
(defun graph-print (graph-id)
    (append (graph-vertices graph-id) (graph-arcs graph-id)))

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
        ((not (gethash heap-id *heaps*))
            (error "Lo heap specificato non esiste.")))
    (cond
        ((= (heap-size heap-id) 0))))

;; Questo predicato è vero quando lo heap heap-id contiene almeno un elemento.
(defun heap-not-empty (heap-id)
    (cond 
        ((not (gethash heap-id *heaps*))
            (error "Lo heap specificato non esiste.")))
    (cond
        ((= (heap-size heap-id) 0) nil)
        (t t)))

;; Ritorna una lista di due elementi dove K è la chiave minima e V il valore associato.
(defun heap-head (heap-id)
    (cond 
        ((not (gethash heap-id *heaps*))
            (error "Lo heap specificato non esiste."))
        ((heap-empty heap-id)
            (error "Lo heap specificato e' vuoto.")))
    (aref (heap-actual-heap heap-id) 0))

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
            (new-heap heap-id))
        ((= (heap-size heap-id) (length (heap-actual-heap heap-id)))
            (setf
                (gethash heap-id *heaps*)
                (list 'heap heap-id 
                    (heap-size heap-id)
                    (adjust-array 
                        (heap-actual-heap heap-id) 
                        (+ (heap-size heap-id) 1))))))
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
            (error "Lo heap specificato non esiste."))
        ((= new-key old-key) t)
        (t (mapheap 
            #'(lambda (heap-entry index) 
                (cond
                    ((= (car heap-entry) old-key)
                        (cond
                            ((numberp V) 
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
                                                    index (heap-size heap-id)))))))
                            (t (cond
                                    ((string= (car (cdr heap-entry)) V)
                                        (setf
                                            (aref (heap-actual-heap heap-id) index)
                                            (list new-key V))
                                        (cond
                                            ((< new-key old-key)
                                                (heapify-insert heap-id index))
                                            ((> new-key old-key)
                                                (heapify (heap-actual-heap heap-id)
                                                    index (heap-size heap-id)))))))))))
            (heap-actual-heap heap-id)
            (heap-size heap-id)))))

;; Stampa sulla console lo stato interno dello heap heap-id.
(defun heap-print (heap-id)
    (cond 
        ((not (gethash heap-id *heaps*))
            (error "Lo heap specificato non esiste.")))
    (let ((heap-entries ()))
        (mapheap 
            #'(lambda (heap-entry index) 
                (push (cons index heap-entry) heap-entries))
            (heap-actual-heap heap-id)
            (heap-size heap-id))
        (print (reverse heap-entries)))
        t)

;; Minimum Spanning Trees
(defparameter *vertex-key* 
    (make-hash-table :test #'equal))

(defparameter *previous* 
    (make-hash-table :test #'equal))

(defparameter *visited* 
    (make-hash-table :test #'equal))

;; Dato un vertex-id di un grafo graph-id ritorna il peso minimo di un arco che connette 
;; vertex-id nell’albero minimo
(defun mst-vertex-key (graph-id vertex-id)
    (or (gethash (list graph-id vertex-id) *vertex-key*)
        most-positive-double-float))

;; Ritorna il vertice U "genitore" di V nel minimum spanning tree V
(defun mst-previous (graph-id V)
    (or (gethash (list graph-id V) *previous*)
        nil))

;; Dopo la sua esecuzione la hash-table *vertex-key* contiene al suo interno le associazioni 
;; (graph-id V) => d per ogni V appartenente a graph-id. La hash-table *previous* contiene le 
;; associazioni (graph-id V) => U calcolate durante l’esecuzione dell’algoritmo di Prim.
(defun heap-old-key (heap size value &optional (index 0))
    (cond
        ((< index size)
            (cond
                ((numberp value)
                    (cond
                        ((= value (car (cdr (aref heap index)))) (car (aref heap index)))
                        (t (heap-old-key heap size value (+ index 1)))))
                (t (cond
                    ((string= value (car (cdr (aref heap index)))) (car (aref heap index)))
                    (t (heap-old-key heap size value (+ index 1)))))))))

(defun mst-update-node (graph-id vertex-id parent-id weight)
    (cond
        ((not (gethash (list graph-id vertex-id) *visited*))
            (let ((old-key (heap-old-key (heap-actual-heap graph-id) (heap-size graph-id) vertex-id)))
                (cond ((not (null old-key)) (cond
                        ((< weight (mst-vertex-key graph-id vertex-id))
                            (setf (gethash (list graph-id vertex-id) *vertex-key*) weight)
                            (setf (gethash (list graph-id vertex-id) *previous*) parent-id)
                            (heap-modify-key graph-id weight old-key vertex-id)))))))))

(defun mst-build-tree (graph-id)
    (cond
        ((heap-not-empty graph-id)
            (let ((head (car (cdr (heap-extract graph-id)))))
                (setf
                    (gethash (list graph-id head) *visited*)
                    (list graph-id head))
                (mapc
                    #'(lambda (arc)
                        (cond
                            ((numberp head)
                                (cond
                                    ((= (car (cdr (cdr arc))) head)
                                        (mst-update-node graph-id (car (cdr (cdr (cdr arc))))
                                            head (car (cdr (cdr (cdr (cdr arc)))))))
                                    (t (mst-update-node graph-id (car (cdr (cdr arc)))
                                        head (car (cdr (cdr (cdr (cdr arc)))))))))
                            (t (cond
                                ((string= (car (cdr (cdr arc))) head)
                                    (mst-update-node graph-id (car (cdr (cdr (cdr arc))))
                                        head (car (cdr (cdr (cdr (cdr arc)))))))
                                (t (mst-update-node graph-id (car (cdr (cdr arc)))
                                    head (car (cdr (cdr (cdr (cdr arc)))))))))))
                    (graph-vertex-neighbors graph-id head)))
            (mst-build-tree graph-id))))

(defun mst-prim (graph-id source)
    (cond 
        ((not (is-graph graph-id))
            (error "Il grafo specificato non esiste."))
        ((not (gethash (list graph-id source) *vertices*))
            (error "Il vertice specificato non esiste.")))
    (mapc
        #'(lambda (vertex)
            (heap-insert graph-id most-positive-double-float 
                (car (cdr (cdr vertex))))) 
        (graph-vertices graph-id))
    (heap-modify-key graph-id 0 most-positive-double-float source)
    (setf (gethash (list graph-id source) *vertex-key*) most-positive-double-float)
    (setf (gethash (list graph-id source) *previous*) nil)
    (mst-build-tree graph-id))

;; Questa funzione ritorna una lista degli archi del MST ordinata secondo un attraversamento 
;; preorder dello stesso, fatta rispetto al peso dell’arco. 
(defun mst-get-childs (graph-id node)
    (let ((childs ()))
        (maphash 
            #'(lambda (key value) 
                (cond 
                    ((not (null value))
                    (cond 
                        ((string= graph-id (car key))
                            (cond 
                                ((numberp node)
                                    (cond
                                        ((= node value) 
                                        (push (list (car (cdr key)) 
                                                (gethash (list graph-id (car (cdr key))) 
                                                    *vertex-key*))
                                            childs))))
                                (t (cond
                                    ((string= node value) 
                                        (push (list (car (cdr key)) 
                                                (gethash (list graph-id (car (cdr key))) 
                                                    *vertex-key*))
                                            childs))))))))))
            *previous*)
        childs))

(defun mst-sort-childs (x y)
    (cond 
        ((> (car (cdr x)) (car (cdr y))) t)
        ((= (car (cdr x)) (car (cdr y)))
            (cond 
                ((numberp (car x))
                    (cond
                        ((> (car x) (car y)) t)))
                (t (cond 
                    ((string> (car x) (car y)) t)))))))

(defun graph-find-arc (graph-id source-vertex-id dest-vertex-id)
    (or (gethash (list graph-id source-vertex-id dest-vertex-id) *arcs*)
        (gethash (list graph-id dest-vertex-id source-vertex-id) *arcs*)))

(defun mst-preorder-tree (graph-id source childs preorder-tree)
    (cond 
        ((not (null (car childs)))
            (let ((sub-tree (mst-preorder-tree graph-id (car (car childs))
                (sort (mst-get-childs graph-id (car (car childs))) 'mst-sort-childs)
                preorder-tree)))
                (push (graph-find-arc graph-id source (car (car childs))) sub-tree)
                (cond 
                    ((not (null (cdr childs)))
                    (mst-preorder-tree graph-id source (cdr childs) sub-tree))
                (t sub-tree))))
        (t preorder-tree)))

(defun mst-get (graph-id source)
    (cond 
        ((not (is-graph graph-id))
            (error "Il grafo specificato non esiste."))
        ((not (null (gethash (list graph-id source) *previous*)))
            (error "Il vertice specificato non e' la radice dell'MST")))
    (mst-preorder-tree graph-id source 
        (sort (mst-get-childs graph-id source) 'mst-sort-childs) ()))
