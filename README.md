# AdvancedDataStructures
A collection of exotic &amp; advanced data structures in C++ and Haskell.

* **SkewHeap**: a self-adjusting, leftist heap where every operation is done via skew heap merging. Although there are no structural constraints, it has been shown that all heap operations work in O(lgn) *amortized* time.
* **Treap**: **tr**ee + h**eap** = treap. A binary ordered tree where every node has a randomly generated "priority". The values in the nodes form a binary ordered tree, while the priorities in the nodes form a binary heap in the same time. Relies on rotations for moving values (with their respective priorities) up and down the structure, while preserving the BOT invariant. Therefore the tree is roughly well balanced and all operations have O(lgn) *expected* time complexity.

To-do:
* fix existing structures
* add moar structures
