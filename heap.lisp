;;;; heap.lisp
;;;;
;;;; Autore: Davide Costantini
;;;;
;;;; Libreria per la gestione di MinHeap.

;;; Tabella contenente le rappresentazioni degli heap.
(defparameter *heaps* (make-hash-table :test #'equal))

(defun new-heap (heap-id &optional (capacity 42))
  "Crea, se non esiste al momento della chiamata, un heap chiamato heap-id e
ritorna la rappresentazione dell'heap."
  (or (gethash heap-id *heaps*)
      (setf 
       (gethash heap-id *heaps*)
       (list 'heap heap-id 0 (make-array capacity)))))

(defun heap-id (heap-id)
  "Ritorna T se esiste un heap di nome heap-id, NIL altrimenti."
  (cond ((gethash heap-id *heaps*) t)))

(defun heap-size (heap-id)
  "Ritorna la dimensione effettiva dell'heap heap-id."
  (car (cdr (cdr (gethash heap-id *heaps*)))))

(defun heap-actual-heap (heap-id)
  "Ritorna l'implementazione effettiva dell'heap heap-id."
  (car (cdr (cdr (cdr (gethash heap-id *heaps*))))))

(defun heap-delete (heap-id)
  "Rimuove l'heap heap-id e ritorna T."
  (remhash heap-id *heaps*))

(defun heap-empty (heap-id)
  "Ritorna T se l'heap heap-id è vuoto, NIL altrimenti."
  (cond ((not (gethash heap-id *heaps*))
         (error "Lo heap specificato non esiste.")))
  (cond ((= (heap-size heap-id) 0))))

(defun heap-not-empty (heap-id)
  "Ritorna T se l'heap heap-id non è vuoto, NIL altrimenti."
  (cond ((not (gethash heap-id *heaps*))
         (error "Lo heap specificato non esiste.")))
  (cond ((= (heap-size heap-id) 0) nil)
        (t t)))

(defun heap-head (heap-id)
  "Ritorna l'elemento in testa all'heap."
  (cond ((not (gethash heap-id *heaps*))
         (error "Lo heap specificato non esiste."))
        ((heap-empty heap-id)
         (error "Lo heap specificato e' vuoto.")))
  (aref (heap-actual-heap heap-id) 0))

(defun heapify-insert (heap-id index)
  "Ritorna T quando l'heap soddisfa la proprietà di heap."
  (let ((heap (heap-actual-heap heap-id)))
    (cond ((= index 0) t)
          (t (let ((current (aref heap index))
                   (parent (aref heap (floor (/ index 2)))))
               (cond ((< (car current) (car parent)) 
                      (setf (aref heap index) parent)
                      (setf (aref heap (floor (/ index 2))) current)
                      (heapify-insert heap-id (floor (/ index 2))))))
             t))))

(defun heap-insert (heap-id K V)
  "Ritorna T quando è stato possibile inserire l'elemento (K V) nell'heap
heap-id e l'heap soddisfa la proprietà di heap."
  (cond ((not (gethash heap-id *heaps*))
         (new-heap heap-id)))
  (let ((heap (heap-actual-heap heap-id))
        (size (heap-size heap-id)))
    (cond ((= size (length heap))
           (setf (gethash heap-id *heaps*)
                 (list 'heap heap-id size (adjust-array heap (+ size 1))))))
    (setf (aref (heap-actual-heap heap-id) size) (list K V))
    (setf (gethash heap-id *heaps*)
          (list 'heap heap-id (+ size 1) (heap-actual-heap heap-id)))
    (heapify-insert heap-id size)))

(defun smallest (heap index-x index-y size)
  "Ritorna l'elemento con chiave più piccola tra quello in posizione index-x
e quello in posizione index-y."
  (cond ((< index-x size)
         (cond ((<= (car (aref heap index-x)) (car (aref heap index-y)))
                index-x)
               (t index-y)))
        (t index-y)))

(defun swap (heap index to-swap)
  "Scambia i valori degli elementi in posizione index e to-swap e ritorna
to-swap"
  (cond ((/= index to-swap)
         (let ((x (aref heap index))
               (y (aref heap to-swap)))
           (setf (aref heap index) y)
           (setf (aref heap to-swap) x))
         to-swap)
        (t -1)))

(defun heapify (heap index size)
  "Ritorna T quando l'heap soddisfa la proprietà di heap."
  (cond ((/= index -1) 
         (heapify heap 
                  (swap heap index 
                        (smallest heap 
                                  (- (+ (* (+ index 1) 2) 1) 1)
                                  (smallest heap 
                                            (- (* (+ index 1) 2) 1) 
                                            index size) 
                                  size))
                  size))))

(defun heap-extract (heap-id)
  "Ritorna l'elemento in testa all'heap heap-id se è stato possibile rimuoverlo
e l'heap soddisfa ancora la proprietà di heap."
  (let ((current-head (heap-head heap-id))
        (heap (heap-actual-heap heap-id))
        (size (heap-size heap-id)))
    (setf (aref heap 0) (aref heap (- size 1)))
    (setf (aref heap (- size 1)) 0)
    (setf (gethash heap-id *heaps*) (list 'heap heap-id (- size 1) heap))
    (cond ((> size 0) 
           (heapify heap 0 (- size 1))))
    current-head))

(defun mapheap (func heap size &optional (index 1))
  "Applica la funzione func ad ogni elemento dell'heap e ritorna NIL."
  (cond ((< (- index 1) size)
         (funcall func (aref heap (- index 1)) (- index 1))
         (mapheap func heap size (* index 2))
         (mapheap func heap size (+ (* index 2) 1)))))

(defun modify-entry (heapid heap size new-key old-key value &optional (index 0))
  "Modifica il primo elemento la cui chiave è old-key ed il valore è value
impostando new-key come chiave."
  (cond ((< index size)
         (cond ((equal (aref heap index) (list old-key value))
                (setf (aref heap index) 
                      (list new-key value))
                (cond ((< new-key old-key)
                       (heapify-insert heapid index) t)
                      ((> new-key old-key)
                       (heapify heap index size) t)))     
               (t (modify-entry heapid heap size new-key old-key value 
                                (+ index 1)))))))

(defun heap-modify-key (heap-id new-key old-key V)
  "Ritorna T se è stato possibile modificare la chiave dell'elemento dell'heap
heap-id con chiave old-key e valore V in new-key e l'heap soddisfa ancora
la proprietà di heap."
  (cond ((not (gethash heap-id *heaps*))
         (error "Lo heap specificato non esiste."))
        ((= new-key old-key) t)
        (t (modify-entry heap-id 
                         (heap-actual-heap heap-id) 
                         (heap-size heap-id) 
                         new-key old-key V))))

(defun heap-print (heap-id)
  "Stampa una lista contenente gli elementi dell'heap heap-id e ritorna T."
  (cond ((not (gethash heap-id *heaps*))
         (error "Lo heap specificato non esiste.")))
  (let ((heap-entries ()))
    (mapheap (lambda (heap-entry index) 
               (push (cons index heap-entry) heap-entries))
             (heap-actual-heap heap-id)
             (heap-size heap-id))
    (print (reverse heap-entries))
    t))
