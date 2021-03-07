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
    Node* parent;
    Node* prev; // left sibling
    Node* next; // right sibling
    Node* firstChild;

    Node(Node* parent, Node* prev, const T& value)
        : value{ value }, parent{ parent }, prev{ prev }, next{ nullptr }, firstChild{ nullptr } {}
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
    // - all children have the right parent (this node)
    // - all children are connected in a doubly linked list
    // - its value in not smaller that its parent's (to-do)
    // - ???
    void validate(const int rank, const Node* parent, const Node* prev) const {
        if constexpr (vassertEnabled) {
            int myRank = getRank();
            vassert(myRank == rank);
            vassert(this->parent == parent);
            vassert(this->prev == prev);
            const Node* prevChild = nullptr;
            for (const Node* child = firstChild; child; child = child->next) {
                child->validate(--myRank, this, prevChild);
                prevChild = child;
            }
        }
    }
    // Performs a deep copy of the a given node & all the reachable nodes from it.
    static Node* deepCopy(const Node* ptr, Node* parent, Node* prev) {
        if (!ptr) {
            return nullptr;
        }
        Node* res = new Node(parent, prev, ptr->value);
        res->firstChild = deepCopy(ptr->firstChild, res, nullptr); // first child has no left sibling (obv.)
        res->next = deepCopy(ptr->next, parent, res); // siblings have the same parent
        return res;
    }
    // Destroys a given node & all the reachable nodes from it (and sets the pointer to null just in case)
    static void deepFree(Node*& ptr) {
        if (!ptr) {
            return;
        }
        ptr->parent = ptr->prev = nullptr; // prevents use-after-free, can be removed
        deepFree(ptr->firstChild);
        deepFree(ptr->next);
        delete ptr;
        ptr = nullptr; // prevents use-after-free, can be removed
    }
};

// Pointer-size pair to a list of binary trees & total value count. Each tree's rank
// corresponds to the index of each set bit in the size, starting from the lowest.
template <typename T>
struct BinomialTreeList {
    Node<T>* first; // Pointer to the first binomial tree in the list
    uint64_t size; // Number of currently contained values

    explicit operator bool() const {
        vassert((first == nullptr) == (size == 0));
        return (first != nullptr);
    }
    // Note: use this instead of manual member manipulation
    // Orphans the first binomial tree from a given heap (to prepare for merging) and
    // advances the heap pointer and corresponding size. Returns pointer to the orphaned tree.
    Node<T>* advance() {
        Node<T>* old = first;
        advancePtr();
        if (first) {
            first->prev = nullptr;
        }
        old->next = nullptr;
        return old;
    }
    // Advances the heap pointer & updates the size correspondingly. Does not destroy links between nodes.
    void advancePtr() {
        first = first->next;
        size &= (size - 1);
    }
    // Get the rank of the first tree in the list
    int getRank() const {
        return std::countr_zero(size);
    }
};

template <typename T>
class BinomialHeap {
    BinomialTreeList<T> trees;

    // Merge two trees with the same rank
    static Node<T>* mergeTrees(Node<T>* lhs, Node<T>* rhs) {
        // Pointers should be to tree root nodes, i.e. no parents or sibling
        vassert(!lhs->parent && !rhs->parent);
        vassert(!lhs->prev && !lhs->next && !rhs->prev && !rhs->next);
        vassert(lhs->getRank() == rhs->getRank());
        if (lhs->value < rhs->value) {
            std::swap(lhs, rhs);
        }
        // lhs should now become the first child of rhs
        Node<T>* child = rhs->firstChild;
        if (child) {
            vassert(child->prev == nullptr);
            child->prev = lhs;
        }
        lhs->next = child;
        lhs->parent = rhs;
        rhs->firstChild = lhs;
        return rhs;
    }
    // Merges two ordered lists of binomial trees, while also adding a "carry" tree,
    // made from two trees previously in these lists, that were of the same rank.
    static Node<T>* mergeWithCarry(Node<T>* const prev, Node<T>* carry, int carryRank, BinomialTreeList<T> lhs, BinomialTreeList<T> rhs) {
        vassert(!(lhs && lhs.first->prev) && !(rhs && rhs.first->prev));
        while (lhs && rhs) {
            const int lhsRank = lhs.getRank();
            const int rhsRank = rhs.getRank();
            vassert(carryRank <= lhsRank && carryRank <= rhsRank);
            // If there are still >1 trees with the same rank => merge them to continue "carrying over"
            if (carryRank == lhsRank && carryRank == rhsRank) {
                Node<T>* newCarry = mergeTrees(lhs.advance(), rhs.advance());
                carry->next = mergeWithCarry(carry, newCarry, carryRank + 1, lhs, rhs);
                carry->prev = prev;
                return carry;
            } else if (carryRank == lhsRank) {
                carry = mergeTrees(carry, lhs.advance());
                ++carryRank;
            } else if (carryRank == rhsRank) {
                carry = mergeTrees(carry, rhs.advance());
                ++carryRank;
            } else {
                // otherwise, just "place" carry bit here & continue normally
                carry->next = mergeHeaps(carry, lhs, rhs);
                carry->prev = prev;
                return carry;
            }
        }
        // If one list's consumed, insert the carry bit into whatever's left from the other
        BinomialTreeList<T>& rest = (lhs ? lhs : rhs);
        while (rest && carryRank == rest.getRank()) {
            vassert(carry->getRank() == carryRank);
            carry = mergeTrees(carry, rest.advance());
            ++carryRank;
        }
        vassert(rest.size == 0 || carryRank < rest.getRank());
        // Push carry to the front of what's left
        carry->prev = prev;
        if (rest.first) {
            rest.first->prev = carry;
            carry->next = rest.first;
        }
        return carry;
    }
    // Classic algorithm of merging two ordered lists - here lists of binomial trees.
    // The trees' ranks in each list can be inferred by the set bit in the corresponding size.
    static Node<T>* mergeHeaps(Node<T>* const prev, BinomialTreeList<T> lhs, BinomialTreeList<T> rhs) {
        vassert(!(lhs.first && lhs.first->prev) && !(rhs.first && rhs.first->prev));
        if (!lhs || !rhs) {
            Node<T>* res = (lhs ? lhs.first : rhs.first);
            res->prev = prev;
            return res;
        }
        const int lhsRank = lhs.getRank();
        const int rhsRank = rhs.getRank();
        vassert(lhsRank == lhs.first->getRank() && rhsRank == rhs.first->getRank());
        if (lhsRank != rhsRank) {
            Node<T>* newFirst = (lhsRank < rhsRank ? lhs.advance() : rhs.advance());
            newFirst->next = mergeHeaps(newFirst, lhs, rhs);
            newFirst->prev = prev;
            return newFirst;
        } else {
            // We cannot have two trees of the same rank -> merge them, forming a "carry" tree,
            // corresponding to having a carry bit while adding the binary numbers lhsSize & rhsSize.
            Node<T>* carry = mergeTrees(lhs.advance(), rhs.advance());
            return mergeWithCarry(prev, carry, lhsRank + 1, lhs, rhs);
        }
    }
public:
    // Standard big 6
    BinomialHeap() : trees{ nullptr, 0 } {}
    BinomialHeap(const BinomialHeap& other)
        : trees{ Node<T>::deepCopy(other.trees.first, nullptr, nullptr), other.trees.size } {}
    BinomialHeap& operator=(const BinomialHeap& other) {
        if (this != &other) {
            Node<T>::deepFree(trees.first);
            trees.first = Node<T>::deepCopy(other.trees.first, nullptr, nullptr);
            trees.size = other.trees.size;
        }
        return *this;
    }
    BinomialHeap(BinomialHeap&& other) noexcept
        : trees{ std::exchange(other.trees.first, nullptr), std::exchange(other.trees.size, 0) } {}
    BinomialHeap& operator=(BinomialHeap&& other) noexcept {
        if (this != &other) {
            Node<T>::deepFree(trees.first);
            trees.first = std::exchange(other.trees.first, nullptr);
            trees.size = std::exchange(other.trees.size, 0);
        }
        return *this;
    }
    ~BinomialHeap() {
        Node<T>::deepFree(trees.first);
        trees.size = 0;
    }
    // to-do: return minPointer->value;
    const T& getMin() const {
        vassert(trees.first); // Can not call this on an empty tree
        Node<T>* res = trees.first;
        for (Node<T>* curr = trees.first->next; curr; curr = curr->next) {
            if (curr->value < res->value) {
                res = curr;
            }
        }
        return res->value;
    }
    // to-do: return a proxy, suitable for decreaseKey()
    void insert(const T& value) {
        Node<T>* singleton = new Node<T>(nullptr /*parent*/, nullptr /*prev*/, value);
        if (!trees.first) {
            /*minPointer =*/ trees.first = singleton;
        } else {
            trees.first = mergeHeaps(nullptr, trees, { singleton, 1 });
            // to-do: if (singleton->value < minPointer->value) { minPointer = singleton; }
            // note: if both are equal, it is not guaranteed which node will be a root (!) be careful
            //       either the two will be roots, or one will be the parent of the other (maybe)
        }
        ++trees.size;
    }
    // Merge another heap into this one & leave the other one empty (hence, the rvalue reference)
    void merge(BinomialHeap&& other) {
        if (!other.trees.first) { // Nothing to do
            return;
        } else if (!trees.first) { // Just swap
            swap(other);
        } else {
            trees.first = mergeHeaps(nullptr, trees, other.trees);
            trees.size += other.trees.size;
            // to-do: fix minPointers
            other.trees = { nullptr, 0 };
        }
    }
    // Swap heap contents with another
    void swap(BinomialHeap& other) {
        std::swap(trees, other.trees);
    }
    // Validates heap structure (requires vassertEnabled). O(n).
    void validate() const {
        if constexpr (vassertEnabled) {
            const Node<T>* prev = nullptr;
            BinomialTreeList<T> copy = trees;
            while (copy.first) {
                vassert(copy.first->prev == prev);
                copy.first->validate(copy.getRank(), nullptr, prev);
                prev = copy.first;
                copy.advancePtr();
            }
        }
    }

    // Full access to heap internals for visualization & debugging
    friend struct Printer;
};