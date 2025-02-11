# Minimum Spanning Trees LISP

Progetto Linguaggi di Programmazione, A.A. 2020/2021  
Libreria Prolog: [Minimum Spanning Trees Prolog](https://github.com/Deivmercer/Minimum-Spanning-Trees-Prolog)

## Introduzione

Un problema che appare spesso sotto varie guise consiste nel connettere diversi “punti” in modo “equivalente”, ad esempio collegandoli con dei fili ma senza creare dei cicli. Un altro problema tipico è quello di calcolare il percorso più breve da un punto a un altro di una mappa.  
Vi sono diversi algoritmi in grado di risolvere questi problemi, noti in letteratura come il “Minimum Spanning Tree” (MST Problem, cfr., [CLR+09] capitolo 23) ed il “Single Source Shortest Path Problem” (SSSP Problem, cfr., [CLR+09] capitolo 24).  
Lo scopo di questo progetto è di implementare l’algoritmo di Prim (cfr., [CLR+09] 23.2) per la soluzione del problema MST per grafi non-diretti e connessi con pesi non negativi.  
Per procedere all’implementazione di questi algoritmi è necessario – e, di fatto, è la parte principale del progetto – produrre un’implementazione di un MINHEAP (o MINPRIORITYQUEUE).  

## Utilizzo

Per utilizzare la libreria MST è necessario definire un grafo usando le funzioni della libreria graph. È possibile caricare uno dei grafi di esempio presenti nella cartella grafi con la seguente funzione:

```Lisp
(load "nome-file.lisp")
```

Dopodiché, per lanciare l'algoritmo di Prim sul grafo che è stato definito, è sufficiente utilizzare le seguenti funzioni:

```Lisp
(mst-prim 'grafo 'vertice-sorgente)   ; Lancia l'algoritmo
(mst-get 'grafo 'vertice-sorgente)    ; Recupera il risultato
```

mst-get ritorna una lista contenente i nodi dell'MST visitati in preordine. Il vertice-sorgente utilizzato nelle due funzioni deve essere identico.

## Riferimenti

[CLR+09] Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, Clifford Stein, Introduction to Algorithms, Third Edition, The MIT Press, 2009 (Capitoli 6 e 23.2)  
[SW11] Robert Sedgewick, Kevin Wayne, Algorithms, Fourth Edition, Addison Wesley Professional, 2011 (Capitoli 2.4 <http://algs4.cs.princeton.edu/24pq/> e 4.4 <http://algs4.cs.princeton.edu/44sp/>)
