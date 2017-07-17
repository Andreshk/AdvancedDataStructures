# AdvancedDataStructures
A collection of exotic &amp; advanced data structures in C++ and Haskell.

* **PairingHeap**: a rather simple, fast and efficient multi-way heap, regarded as a simplified Fibonacci heap. Theoretically slower, but in practise much simpler and therefore more efficient than Fibonacci &amp; Binomial heaps, this is most often the best choice for classical algorithms such as Prim's and Dijkstra's. Has user-friendly interface with `decreaseKey` functionality. Time complexities for `insert`, `extractMin` and `decreaseKey` are O(1), O(lgn) and O(lgn) *amortized*, respectively.
* **SkewHeap**: a self-adjusting, leftist heap where every operation is done via skew heap merging. Although there are no structural constraints, it has been shown that all heap operations work in O(lgn) *amortized* time.
* **Treap**: **tr**ee + h**eap** = treap. A binary ordered tree where every node has a randomly generated "priority". The values in the nodes form a binary ordered tree, while the priorities in the nodes form a binary heap in the same time. Relies on rotations for moving values (with their respective priorities) up and down the structure, while preserving the BOT invariant. Therefore the tree is roughly well balanced and all operations have O(lgn) *expected* time complexity.

To-do:
* fix existing structures
* add moar structures
