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
    Node* next; // right sibling
    Node* firstChild;

    Node(Node* parent, const T& value)
        : value{ value }, parent{ parent }, next{ nullptr }, firstChild{ nullptr } {}
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
    // - its value is not smaller that its parent's
    // - ???
    void validate(const int rank, const Node* parent) const {
        if constexpr (vassertEnabled) {
            int myRank = getRank();
            vassert(myRank == rank);
            vassert(this->parent == parent);
            for (const Node* child = firstChild; child; child = child->next) {
                vassert(!(child->value < this->value));
                child->validate(--myRank, this);
            }
        }
    }
    // Performs a deep copy of the a given node & all the reachable nodes from it.
    static Node* deepCopy(const Node* ptr, Node* parent) {
        if (!ptr) {
            return nullptr;
        }
        Node* res = new Node(parent, ptr->value);
        res->firstChild = deepCopy(ptr->firstChild, res); // first child has no left sibling (obv.)
        res->next = deepCopy(ptr->next, parent); // siblings have the same parent
        return res;
    }
    // Destroys a given node & all the reachable nodes from it (and sets the pointer to null just in case)
    static void deepFree(Node*& ptr) {
        if (!ptr) {
            return;
        }
        ptr->parent = nullptr; // prevents use-after-free, can be removed
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
    Node<T>* removeFirst() {
        Node<T>* res = first;
        advance();
        res->next = nullptr;
        return res;
    }
    // Advances the heap pointer & updates the size correspondingly. Does not destroy links between nodes.
    void advance() {
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
        vassert(!lhs->next && !rhs->next);
        vassert(lhs->getRank() == rhs->getRank());
        if (lhs->value < rhs->value) {
            std::swap(lhs, rhs);
        }
        // lhs should now become the first child of rhs
        Node<T>* child = rhs->firstChild;
        lhs->next = child;
        lhs->parent = rhs;
        rhs->firstChild = lhs;
        return rhs;
    }
    // Merges two ordered lists of binomial trees, while also adding a "carry" tree,
    // made from two trees previously in these lists, that were of the same rank.
    static Node<T>* mergeWithCarry(Node<T>* carry, int carryRank, BinomialTreeList<T> lhs, BinomialTreeList<T> rhs) {
        while (lhs && rhs) {
            const int lhsRank = lhs.getRank();
            const int rhsRank = rhs.getRank();
            vassert(carryRank <= lhsRank && carryRank <= rhsRank);
            // If there are still >1 trees with the same rank => merge them to continue "carrying over"
            if (carryRank == lhsRank && carryRank == rhsRank) {
                Node<T>* newCarry = mergeTrees(lhs.removeFirst(), rhs.removeFirst());
                carry->next = mergeWithCarry(newCarry, carryRank + 1, lhs, rhs);
                return carry;
            } else if (carryRank == lhsRank) {
                carry = mergeTrees(carry, lhs.removeFirst());
                ++carryRank;
            } else if (carryRank == rhsRank) {
                carry = mergeTrees(carry, rhs.removeFirst());
                ++carryRank;
            } else {
                // otherwise, just "place" carry bit here & continue normally
                carry->next = mergeHeaps(lhs, rhs);
                return carry;
            }
        }
        // If one list's consumed, insert the carry bit into whatever's left from the other
        BinomialTreeList<T>& rest = (lhs ? lhs : rhs);
        while (rest && carryRank == rest.getRank()) {
            vassert(carry->getRank() == carryRank);
            carry = mergeTrees(carry, rest.removeFirst());
            ++carryRank;
        }
        vassert(rest.size == 0 || carryRank < rest.getRank());
        // Push carry to the front of what's left
        carry->next = rest.first;
        return carry;
    }
    // Classic algorithm of merging two ordered lists - here lists of binomial trees.
    // The trees' ranks in each list can be inferred by the set bit in the corresponding size.
    static Node<T>* mergeHeaps(BinomialTreeList<T> lhs, BinomialTreeList<T> rhs) {
        if (!lhs || !rhs) {
            return (lhs ? lhs.first : rhs.first);
        }
        const int lhsRank = lhs.getRank();
        const int rhsRank = rhs.getRank();
        vassert(lhsRank == lhs.first->getRank() && rhsRank == rhs.first->getRank());
        if (lhsRank != rhsRank) {
            Node<T>* newFirst = (lhsRank < rhsRank ? lhs.removeFirst() : rhs.removeFirst());
            newFirst->next = mergeHeaps(lhs, rhs);
            return newFirst;
        } else {
            // We cannot have two trees of the same rank -> merge them, forming a "carry" tree,
            // corresponding to having a carry bit while adding the binary numbers lhsSize & rhsSize.
            Node<T>* carry = mergeTrees(lhs.removeFirst(), rhs.removeFirst());
            return mergeWithCarry(carry, lhsRank + 1, lhs, rhs);
        }
    }
    // Returns the address of the pointer to the binomial tree, whose root is the minimum value.
    // This aids removing this tree from the singly-linked list in which it resides.
    Node<T>* const* getMinHelper() const {
        Node<T>* min = trees.first; // Pointer to node with smallest value
        Node<T>* minPrev = nullptr; // Pointer to node before min
        for (Node<T>* prev = trees.first, *curr = prev->next; curr; prev = curr, curr = curr->next) {
            if (curr->value < min->value) {
                min = curr;
                minPrev = prev;
            }
        }
        vassert(!minPrev || minPrev->next == min);
        return (minPrev ? &minPrev->next : &trees.first);
    }
public:
    // Standard big 6
    BinomialHeap() : trees{ nullptr, 0 } {}
    BinomialHeap(const BinomialHeap& other)
        : trees{ Node<T>::deepCopy(other.trees.first, nullptr), other.trees.size } {}
    BinomialHeap& operator=(const BinomialHeap& other) {
        if (this != &other) {
            Node<T>::deepFree(trees.first);
            trees.first = Node<T>::deepCopy(other.trees.first, nullptr);
            trees.size = other.trees.size;
        }
        return *this;
    }
    BinomialHeap(BinomialHeap&& other) noexcept
        : trees{ std::exchange(other.trees, BinomialTreeList<T>{ nullptr, 0 }) } {}
    BinomialHeap& operator=(BinomialHeap&& other) noexcept {
        if (this != &other) {
            Node<T>::deepFree(trees.first);
            trees = std::exchange(other.trees, BinomialTreeList<T>{ nullptr, 0 });
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
        return (*getMinHelper())->value;
    }
    // to-do: return a proxy, suitable for decreaseKey()
    void insert(const T& value) {
        Node<T>* singleton = new Node<T>(nullptr /*parent*/, value);
        if (!trees.first) {
            /*minPointer =*/ trees.first = singleton;
        } else {
            trees.first = mergeHeaps(trees, { singleton, 1 });
            // to-do: if (singleton->value < minPointer->value) { minPointer = singleton; }
            // note: if both are equal, it is not guaranteed which node will be a root (!) be careful
            //       either the two will be roots, or one will be the parent of the other (maybe)
        }
        ++trees.size;
    }
    // Remove the minimum value form the heap
    T extractMin() {
        // we're in a non-const function anyway, so it's ok to const_cast away the result from a const one
        Node<T>** minPtr = const_cast<Node<T>**>(getMinHelper());
        Node<T>* min = *minPtr;
        T res = std::move(min->value);
        // Remove this tree from the list
        *minPtr = min->next;
        // Reverse the min root's children into a self-sufficient tree list
        Node<T>* children = nullptr;
        Node<T>* curr = min->firstChild;
        int minRank = 0;
        while (curr) {
            curr->parent = nullptr;
            Node<T>* next = std::exchange(curr->next, nullptr);
            curr->next = children;
            children = curr;
            curr = next;
            ++minRank;
        }
        // We can now free this
        delete min;
        // Merge the two (sizes should be correct for this step)
        const uint64_t numChildren = (uint64_t(1) << minRank) - 1;
        trees.size -= (numChildren + 1);
        trees.first = mergeHeaps(trees, { children, numChildren });
        trees.size += numChildren;
        return res;
    }
    // Merge another heap into this one & leave the other one empty (hence, the rvalue reference)
    void merge(BinomialHeap&& other) {
        if (!other.trees.first) { // Nothing to do
            return;
        } else if (!trees.first) { // Just swap
            swap(other);
        } else {
            trees.first = mergeHeaps(trees, other.trees);
            trees.size += other.trees.size;
            // to-do: fix minPointers
            other.trees = { nullptr, 0 };
        }
    }
    // Number of values in the heap
    uint64_t size() const { return trees.size; }
    bool empty() const { return !trees; }
    // Swap heap contents with another
    void swap(BinomialHeap& other) {
        std::swap(trees, other.trees);
    }
    // Validates heap structure (requires vassertEnabled). O(n).
    void validate() const {
        if constexpr (vassertEnabled) {
            BinomialTreeList<T> copy = trees;
            while (copy.first) {
                copy.first->validate(copy.getRank(), nullptr);
                copy.advance();
            }
        }
    }

    // Full access to heap internals for visualization & debugging
    friend struct Printer;
};