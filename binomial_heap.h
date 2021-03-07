#pragma once
#include <cstdint> // uint64_t
#include <utility> // std::exchange
#include <bit> // std::countr_zero
#include "vassert.h"

// Binomial tree node. Must keep pointers to all neighboring
// nodes in order to support time-efficient operations.
template <typename T>
struct Node {
    T value;
    Node* next;
    Node* firstChild;

    Node(const T& value) : value{ value }, next{ nullptr }, firstChild{ nullptr } {}
public:
    // Calculate the rank of a binomial tree (<=> number of children).
    // To be used for debugging purposes only (!)
    int getRank() const {
        int res = 0;
        for (const Node* child = firstChild; child; child = child->next) {
            ++res;
        }
        return res;
    }
    // Performs validity checks for a tree root node, such as:
    // - it has the rank it's supposed to have
    // - all children have decreasing (and correct) ranks
    // - all children have the right parent (this node) (to-do)
    // - all children are connected in a doubly linked list (to-do)
    // - its value in not smaller that its parent's
    // - ???
    void validate(const int rank) const {
        if constexpr (vassertEnabled) {
            int myRank = getRank();
            vassert(myRank == rank);
            for (const Node* child = firstChild; child; child = child->next) {
                child->validate(--myRank);
            }
        }
    }
    // Performs a deep copy of the a given node & all the reachable nodes from it.
    static Node* deepCopy(const Node* ptr) {
        if (!ptr) {
            return nullptr;
        }
        Node* res = new Node(ptr->value);
        res->firstChild = deepCopy(ptr->firstChild);
        res->next = deepCopy(ptr->next);
        return res;
    }
    // Destroys a given node & all the reachable nodes from it (and sets the pointer to null just in case)
    static void deepFree(Node*& ptr) {
        if (!ptr) {
            return;
        }
        deepFree(ptr->firstChild);
        deepFree(ptr->next);
        delete ptr;
        ptr = nullptr; // prevents use-after-free, can be removed
    }
};

template <typename T>
class BinomialHeap {
    Node<T>* firstTree; // Pointer to the first binomial tree in the heap
    uint64_t count; // Number of currently contained values

    // Merge two trees with the same rank
    static Node<T>* mergeTrees(Node<T>* lhs, Node<T>* rhs) {
        // Pointers should be to tree root nodes, i.e. no parents or sibling
        //vassert(!lhs->parent && !rhs->parent);
        vassert(/*!lhs->left &&*/ !lhs->next && /*!rhs->left &&*/ !rhs->next);
        vassert(lhs->getRank() == rhs->getRank());
        if (lhs->value < rhs->value) {
            std::swap(lhs, rhs);
        }
        // lhs should now become the first child of rhs
        Node<T>* child = rhs->firstChild;
        /*if (child) {
            vassert(child->left == nullptr);
            child->left = lhs;
        }*/
        lhs->next = child;
        //lhs->parent = rhs;
        rhs->firstChild = lhs;
        return rhs;
    }
    // Orphans the first binomial tree from a given heap (to prepare for merging) and
    // advances the heap pointer and corresponding size. Returns pointer to the orphaned tree.
    static Node<T>* advance(Node<T>*& heap, uint64_t& heapSize) {
        Node<T>* old = std::exchange(heap, heap->next);
        old->next = nullptr;
        heapSize &= (heapSize - 1);
        return old;
    }
    // Merges two ordered lists of binomial trees, while also adding a "carry" tree,
    // made from two trees previously in these lists, that were of the same rank.
    static Node<T>* mergeWithCarry(Node<T>* carry, int carryRank, Node<T>* lhs, uint64_t lhsSize, Node<T>* rhs, uint64_t rhsSize) {
        while (lhs && rhs) {
            const int lhsRank = std::countr_zero(lhsSize);
            const int rhsRank = std::countr_zero(rhsSize);
            vassert(carryRank <= lhsRank && carryRank <= rhsRank);
            // If there are still >1 trees with the same rank => merge them to continue "carrying over"
            if (carryRank == lhsRank && carryRank == rhsRank) {
                Node<T>* newCarry = mergeTrees(advance(lhs, lhsSize), advance(rhs, rhsSize));
                carry->next = mergeWithCarry(newCarry, carryRank + 1, lhs, lhsSize, rhs, rhsSize);
                return carry;
            } else if (carryRank == lhsRank) {
                carry = mergeTrees(carry, advance(lhs, lhsSize));
                ++carryRank;
            } else if (carryRank == rhsRank) {
                carry = mergeTrees(carry, advance(rhs, rhsSize));
                ++carryRank;
            } else {
                // otherwise, just "place" carry bit here & continue normally
                carry->next = mergeHeaps(lhs, lhsSize, rhs, rhsSize);
                return carry;
            }
        }
        // Adds the carried over amount to a given heap.
        auto insertCarry = [&](Node<T>*& heap, uint64_t& heapSize) {
            while (heap && carryRank == std::countr_zero(heapSize)) {
                vassert(carry->getRank() == carryRank);
                carry = mergeTrees(carry, advance(heap, heapSize));
                ++carryRank;
            }
            vassert(heapSize == 0 || carryRank < std::countr_zero(heapSize));
            if (!heap) {
                vassert(heapSize == 0);
                heap = carry;
            } else {
                carry->next = std::exchange(heap, carry);
            }
        };
        // If one heap's consumed, insert the carry bit into whatever's left from the other
        if (!lhs) {
            vassert(lhsSize == 0);
            insertCarry(rhs, rhsSize);
            return rhs;
        } else {
            vassert(rhsSize == 0);
            insertCarry(lhs, lhsSize);
            return lhs;
        }
    }
    // Classic algorithm of merging two ordered lists - here lists of binomial trees.
    // The trees' ranks in each list can be inferred by the set bit in the corresponding size.
    static Node<T>* mergeHeaps(Node<T>* lhs, uint64_t lhsSize, Node<T>* rhs, uint64_t rhsSize) {
        if (!lhs) {
            vassert(lhsSize == 0);
            return rhs;
        } else if (!rhs) {
            vassert(rhsSize == 0);
            return lhs;
        }
        const int lhsRank = std::countr_zero(lhsSize);
        const int rhsRank = std::countr_zero(rhsSize);
        vassert(lhsRank == lhs->getRank() && rhsRank == rhs->getRank());
        if (lhsRank < rhsRank) {
            lhs->next = mergeHeaps(lhs->next, lhsSize & (lhsSize - 1), rhs, rhsSize);
            return lhs;
        } else if (rhsRank < lhsRank) {
            rhs->next = mergeHeaps(lhs, lhsSize, rhs->next, rhsSize & (rhsSize - 1));
            return rhs;
        } else {
            // We cannot have two trees of the same rank -> merge them, forming a "carry" tree,
            // corresponding to having a carry bit while adding the binary numbers lhsSize & rhsSize.
            Node<T>* carry = mergeTrees(advance(lhs, lhsSize), advance(rhs, rhsSize));
            return mergeWithCarry(carry, lhsRank + 1, lhs, lhsSize, rhs, rhsSize);
        }
    }
public:
    // Standard big 6
    BinomialHeap() : firstTree{ nullptr }, count{ 0 } {}
    BinomialHeap(const BinomialHeap& other)
        : firstTree{ Node<T>::deepCopy(other.firstTree) }, count{ count } {}
    BinomialHeap& operator=(const BinomialHeap& other) {
        if (this != &other) {
            Node<T>::deepFree(firstTree);
            firstTree = Node<T>::deepCopy(other.firstTree);
            count = other.count;
        }
        return *this;
    }
    BinomialHeap(BinomialHeap&& other) noexcept
        : firstTree{ std::exchange(other.firstTree, nullptr) }, count{ std::exchange(other.count, 0) } {}
    BinomialHeap& operator=(BinomialHeap&& other) noexcept {
        if (this != &other) {
            Node<T>::deepFree(firstTree);
            firstTree = std::exchange(other.firstTree, nullptr);
            count = std::exchange(other.count, 0);
        }
        return *this;
    }
    ~BinomialHeap() {
        Node<T>::deepFree(firstTree);
        count = 0;
    }
    // to-do: return minPointer->value;
    const T& getMin() const {
        vassert(firstTree); // Can not call this on an empty tree
        Node<T>* res = firstTree;
        for (Node<T>* curr = firstTree->right; curr; curr = curr->right) {
            if (curr->value < res->value) {
                res = curr;
            }
        }
        return res->value;
    }
    // to-do: return a proxy, suitable for decreaseKey()
    void insert(const T& value) {
        Node<T>* singleton = new Node(value);
        if (!firstTree) {
            /*minPointer =*/ firstTree = singleton;
        } else {
            firstTree = mergeHeaps(firstTree, count, singleton, 1);
            // to-do: if (singleton->value < minPointer->value) { minPointer = singleton; }
            // note: if both are equal, it is not guaranteed which node will be a root (!) be careful
            //       either the two will be roots, or one will be the parent of the other (maybe)
        }
        ++count;
    }
    // Merge another heap into this one & leave the other one empty (hence, the rvalue reference)
    void merge(BinomialHeap&& other) {
        if (!other.firstTree) { // Nothing to do
            return;
        } else if (!firstTree) { // Just swap
            swap(other);
        } else {
            firstTree = mergeHeaps(firstTree, count, other.firstTree, other.count);
            count += other.count;
            // to-do: fix minPointers
            other.firstTree = nullptr;
            other.count = 0;
        }
    }
    // Swap heap contents with another
    void swap(BinomialHeap& other) {
        std::swap(firstTree, other.firstTree);
        std::swap(count, other.count);
    }
    // Validates heap structure (requires vassertEnabled). O(n).
    void validate() const {
        if constexpr (vassertEnabled) {
            const Node<T>* t = firstTree;
            uint64_t count1 = count;
            while (t) {
                t->validate(std::countr_zero(count1));
                t = t->next;
                count1 &= (count1 - 1);
            }
        }
    }

    // Full access to heap internals for visualization & debugging
    friend struct Printer;
};