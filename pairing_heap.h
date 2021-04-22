#pragma once
#include <cstddef>    // size_t
#include <algorithm>  // std::swap
#include <functional> // std::less
#include <utility>    // std::exchange
#include "vassert.h"

// Controls whether to use a recursive or iterative approach for the
// pairwise merge, specific to this heap's extractMin procedure.
static constexpr bool useRecursivePairwiseMerge = false;

template <typename T>
struct Node {
    T value;
    Node* leftChild;
    Node* rightSibling;
    Node* predecessor; // left sibling, or parent if none
    Node(const T& val, Node* pred = nullptr)
        : value{ val }, leftChild{ nullptr }, rightSibling{ nullptr }, predecessor{ pred } {}

    // Helper copying/deleting functions
    static Node* copy(const Node*, Node*);
    static void free(const Node*);
    // Merge two trees
    template <typename Compare>
    static Node* merge(Node*, Node*, Compare&);
    // Recursive pairwise merge of a list of siblings (controlled by useRecursivePairwiseMerge)
    template <typename Compare>
    static Node* recursivePairwiseMerge(Node*, Compare&);
    template <typename Compare>
    static Node* iterativePairwiseMerge(Node*, Compare&);
};

template <typename T, typename Compare = std::less<T>>
class PairingHeap {
    // Invariant: root->predecessor == root->rightSibling == nullptr
    Node<T>* root;
    size_t count;
    Compare cmp;

    void copyFrom(const PairingHeap&);
public:
    // Represents an "iterator" or key, returned at insertion,
    // which can later be used to decrease the inserted value.
    class proxy {
        friend class PairingHeap<T, Compare>; // Only a heap can construct proxies to itself
        Node<T>* ptr;

        explicit proxy(Node<T>* ptr) : ptr{ ptr } {}
    public:
        proxy() : ptr{ nullptr } {}
        const T& operator*()  const { return  ptr->value; }
        const T* operator->() const { return &ptr->value; }
        operator bool() const { return bool(ptr); }
        bool operator==(const proxy& other) const { return ptr == other.ptr; }
        bool operator!=(const proxy& other) const { return ptr != other.ptr; }
    };

    // Standard big 6.
    explicit PairingHeap(const Compare& cmp = Compare{}) noexcept
        : root{ nullptr }, count{ 0 }, cmp{ cmp } {}
    PairingHeap(const PairingHeap& other) : PairingHeap{} { copyFrom(other); }
    PairingHeap(PairingHeap&& other) noexcept : PairingHeap{} { std::swap(*this, other); }
    PairingHeap& operator=(const PairingHeap&);
    PairingHeap& operator=(PairingHeap&&) noexcept;
    ~PairingHeap() { clear(); }

    // Merge the contents of a heap into this one & leaves it empty.
    // API makes it clear that after calling x.merge(std::move(y)) y will be moved-from, i.e. empty.
    void merge(PairingHeap&&);

    // Inserting a value - creates a singleton heap and merges with the current heap
    proxy insert(const T&);

    // Standard operation; undefined behaviour for empty heaps
    const T& peek() const { return root->value; }

    // The most complex operation: removing the root and merging all of its children
    T extractMin();

    // Attempts decreasing the value, pointed to by a given proxy, with a new one.
    // Returns whether this was successful (i.e. the provided value was smaller).
    bool decreaseKey(proxy, const T&);

    // More standard methods
    size_t size() const { return count; }
    bool empty() const { return (root == nullptr); }
    void clear();
};

template <typename T>
Node<T>* Node<T>::copy(const Node* ptr, Node* pred) {
    if (!ptr) {
        return nullptr;
    }
    Node* tmp = new Node(ptr->value, pred);
    tmp->leftChild = copy(ptr->leftChild, tmp);
    tmp->rightSibling = copy(ptr->rightSibling, tmp);
    return tmp;
}

template <typename T>
void Node<T>::free(const Node* ptr) {
    if (ptr) {
        free(ptr->leftChild);
        free(ptr->rightSibling);
        delete ptr;
    }
}

template <typename T>
template <typename Compare>
Node<T>* Node<T>::merge(Node* root1, Node* root2, Compare& cmp) {
    // Precautionary
    vassert(!root1->rightSibling && !root1->predecessor);
    vassert(!root2->rightSibling && !root2->predecessor);
    // For simplicity, let root1 be the heap that "adopts" the other
    if (cmp(root2->value, root1->value)) {
        std::swap(root1, root2);
    }
    // Cache the old left child
    root2->rightSibling = root1->leftChild;
    // The left child of the root will be changed, so the old one has to know
    if (root1->leftChild) {
        root1->leftChild->predecessor = root2;
    }
    // Finally, link the two root nodes
    root1->leftChild = root2;
    root2->predecessor = root1;
    // Return the root of the merged heap
    return root1;
}

template <typename T>
template <typename Compare>
Node<T>* Node<T>::recursivePairwiseMerge(Node* first, Compare& cmp) {
    vassert(!(first && first->predecessor));
    if (!first || !first->rightSibling) {
        return first;
    }
    // Isolate the first two siblings properly from the remaining list
    Node* second = std::exchange(first->rightSibling, nullptr);
    second->predecessor = nullptr;
    Node* rest = std::exchange(second->rightSibling, nullptr);
    if (rest) { rest->predecessor = nullptr; }
    // Recursively merge the rest, if any
    rest = recursivePairwiseMerge(rest, cmp);
    Node* firstTwo = merge(first, second, cmp);
    // Combine the merged first two siblings w/ the merged remaining
    if (rest) {
        return merge(firstTwo, rest, cmp);
    } else {
        return firstTwo;
    }
}

template <typename T>
template <typename Compare>
Node<T>* Node<T>::iterativePairwiseMerge(Node* first, Compare& cmp) {
    // First build the list of merged pairs, keeping a pointer to its rightmost member
    Node* last = nullptr;
    while (first) {
        vassert(!first->predecessor && !(last && last->rightSibling));
        if (!first->rightSibling) { // No sibling to pair it with, nothing more to do
            first->predecessor = last;
            last = first;
            break;
        }
        // Isolate the first two siblings properly (some of these assignments might be unnecessary, yes)
        Node* second = std::exchange(first->rightSibling, nullptr);
        second->predecessor = nullptr;
        Node* rest = std::exchange(second->rightSibling, nullptr);
        if (rest) { rest->predecessor = nullptr; }
        // Merge this pair & "insert" it to the back of the merged pairs list
        Node* firstTwo = merge(first, second, cmp);
        firstTwo->predecessor = last;
        last = firstTwo;
        // Continue to the next sibling to be merged
        first = rest;
    }
    // Now that we have a pointer to the last pair, merge back into one
    while (last->predecessor) {
        vassert(!last->rightSibling);
        // Again, temporarily isolate the two to be merged (just in case)
        Node* prev = std::exchange(last->predecessor, nullptr);
        prev->rightSibling = nullptr;
        Node* rest = std::exchange(prev->predecessor, nullptr);
        if (rest) { rest->rightSibling = nullptr; }
        // Merge the two & reattach them to the back of the list
        last = merge(last, prev, cmp);
        last->predecessor = rest;
        if (rest) {
            rest->rightSibling = last;
        }
    }
    // We now have one tree remaining
    return last;
}

template <typename T, typename Compare>
void PairingHeap<T, Compare>::copyFrom(const PairingHeap& other) {
    vassert(!root);
    root  = Node<T>::copy(other.root, nullptr);
    count = other.count;
    cmp   = other.cmp;
}

template <typename T, typename Compare>
auto PairingHeap<T, Compare>::operator=(const PairingHeap& other) -> PairingHeap& {
    if (this != &other) {
        clear();
        copyFrom(other);
    }
    return *this;
}

template <typename T, typename Compare>
auto PairingHeap<T, Compare>::operator=(PairingHeap&& other) noexcept -> PairingHeap& {
    if (this != &other) {
        clear();
        std::swap(*this, other);
    }
    return *this;
}

template <typename T, typename Compare>
void PairingHeap<T, Compare>::merge(PairingHeap&& other) {
    // Some edge cases are handled way earlier
    if (this == &other || other.empty()) {
        // nothing to do
    } else if (empty()) {
        std::swap(*this, other);
    } else {
        root = Node<T>::merge(root, other.root, cmp);
        count += other.count;
        other.root  = nullptr;
        other.count = 0;
    }
}

template <typename T, typename Compare>
auto PairingHeap<T, Compare>::insert(const T& value) -> proxy {
    // Simple: make a new heap and merge it
    Node<T>* res = new Node<T>(value);
    root = (count == 0 ? res : Node<T>::merge(root, res, cmp));
    ++count;
    return proxy{ res };
}

template <typename T, typename Compare>
T PairingHeap<T, Compare>::extractMin() {
    // Save the root's value & leftChild before freeing the node
    const T result = peek();
    Node<T>* first = root->leftChild;
    delete root;
    root = nullptr;
    // If no children, nothing else to do
    if (first) {
        first->predecessor = nullptr;
        // Merge children in pairs & the pairs back-to-front, recursively or iteratively
        if constexpr (useRecursivePairwiseMerge) {
            root = Node<T>::recursivePairwiseMerge(first, cmp);
        } else {
            root = Node<T>::iterativePairwiseMerge(first, cmp);
        }
    }
    vassert(!(root && (root->predecessor || root->rightSibling)));
    --count;
    return result;
}

template <typename T, typename Compare>
bool PairingHeap<T, Compare>::decreaseKey(proxy pr, const T& newKey) {
    // If the proxy does not point to a node in this heap -> undefined behaviour(!)
    if (!cmp(newKey, *pr)) {
        return false;
    }
    // Update the value
    Node<T>* location = pr.ptr;
    location->value = newKey;
    // If the value is at the root (<=> no predecessor), nothing more to change
    if (location == root) {
        return true;
    }
    // Tell its left sibling/parent it has a new right sibling/left child
    if (location == location->predecessor->rightSibling) {
        location->predecessor->rightSibling = location->rightSibling;
    } else {
        location->predecessor->leftChild = location->rightSibling;
    }
    // Tell its right sibling (if any) it has a new left sibling
    if (location->rightSibling) {
        location->rightSibling->predecessor = location->predecessor;
    }
    // Isolate the current node as a root of a new heap
    location->rightSibling = location->predecessor = nullptr;
    // Merge this heap back into the remainder
    root = Node<T>::merge(root, location, cmp);
    return true;
}

template <typename T, typename Compare>
void PairingHeap<T, Compare>::clear() {
    Node<T>::free(root);
    root  = nullptr;
    count = 0;
}
