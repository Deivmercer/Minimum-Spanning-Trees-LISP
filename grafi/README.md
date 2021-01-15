[grafo]: time_grafo.png  
[primkiller_10k]: time_primkiller_10k.png
[primkiller_50k]: time_primkiller_50k.png
[primkiller_500k]: time_primkiller_500k.png

# Minimum Spanning Trees Prolog

Il grafo contenuto in grafo.csv è stato preso da [CLR+09] capitolo capitolo 23. Si tratta di un semplice grafo su cui testare.  
I grafi contenuti in primkiller_10k.lisp, primkiller_50k.lisp e primkiller_500k.lisp sono stati generati da un compagno di corso. Si tratta di grafi rispettivamente da 10.000, 50.000 e 500.000 archi su cui si possono fare effettivi test sulla performance dell'implementazione dell'algoritmo.  

## Benchmarks

N.B.: I tempi di esecuzione possono variare a seconda del sistema in cui il programma viene eseguito.

### Grafo

mst_prim:   0.0 secondi  
mst_get:    0.0 secondi  

![grafo]

### Primkiller 10k

mst_prim:   0.6 secondi  
mst_get:    0.0 secondi  

![primkiller_10k]

### Primkiller 50k

mst_prim:   17  secondi  
mst_get:    0.6 secondi  

![primkiller_10k]

### Primkiller 500k

mst_prim:   187 secondi  
mst_get:    2.7 secondi  

![primkiller_10k]
