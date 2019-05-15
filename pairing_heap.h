#pragma once
#include <vector>
#include <cstddef>    // size_t
#include <algorithm>  // std::swap
#include <functional> // std::less

template<class T, class Compare = std::less<T>>
class PairingHeap {
    struct node {
        T value;
        node* leftChild;
        node* rightSibling;
        node* predecessor; // parent or left sibling
        node(const T& val, node* pred = nullptr) 
            : value{ val }, leftChild{ nullptr }, rightSibling{ nullptr }, predecessor{ pred } {}
    };

    // Invariant: root->predecessor == root->rightSibling == nullptr
    node* root;
    size_t count;
    Compare cmp;

    // Helper copying/deleting functions
    static node* copyNode(const node*, node*);
    static void freeNode(const node*);
    void copyFrom(const PairingHeap&);

    // Constructing a heap form a root pointer (received, perhaps, from a child node)
    // without knowing the heap's size. Used for making a singleton heap.
    explicit PairingHeap(node* root, size_t count = 0, const Compare& cmp = Compare{})
        : root{ root }, count{ count }, cmp{ cmp } {}
public:
    class proxy {
        friend class PairingHeap<T, Compare>;
        node* ptr;

        explicit proxy(node* ptr) : ptr{ ptr } {}
    public:
        proxy() : ptr{ nullptr } {}
        const T& operator*()  const { return  ptr->value; }
        const T* operator->() const { return &ptr->value; }
        operator bool() const { return bool(ptr); }
        bool operator==(const proxy& other) const { return ptr == other.ptr; }
        bool operator!=(const proxy& other) const { return ptr != other.ptr; }
    };

    // Standard big 6.
    // Move ctor _must_ be noexcept is order for std::vector<PairingHeap>::push_back()
    // to work properly (move heaps, not copy them) during extractMin().
    PairingHeap(const Compare& cmp = Compare{}) noexcept : PairingHeap{ nullptr, 0, cmp } {}
    PairingHeap(const PairingHeap& other) : PairingHeap{} { copyFrom(other); }
    PairingHeap(PairingHeap&& other) noexcept : PairingHeap{} { swap(other); }
    PairingHeap& operator=(const PairingHeap&);
    PairingHeap& operator=(PairingHeap&&);
    ~PairingHeap() { clear(); }

    // x.merge(y) always leaves y empty (!) regardless which root value
    // is smaller. Afterwards x contains all values of both x and y.
    void merge(PairingHeap&);

    // Inserting a value - creates a singleton heap and merges with the current heap
    proxy insert(const T&);

    // Standard operation; undefined behaviour for empty heaps
    const T& peek() const { return root->value; }

    // The most complex operation: removing the root and merging all of its children
    T extractMin();

    // Special (!)
    proxy decreaseKey(proxy, const T&);

    // More standard methods
    size_t size() const { return count; }
    bool empty() const { return (root == nullptr); }
    void clear();

    // For convenience
    void swap(PairingHeap&) noexcept;
};

template<class T, class Compare>
auto PairingHeap<T, Compare>::copyNode(const node* ptr, node* _pred) -> node* {
    if (!ptr) {
        return nullptr;
    }
    node* tmp = new node(ptr->value, _pred);
    tmp->leftChild = copyNode(ptr->leftChild, tmp);
    tmp->rightSibling = copyNode(ptr->rightSibling, tmp);
    return tmp;
}

template<class T, class Compare>
void PairingHeap<T, Compare>::freeNode(const node* ptr) {
    if (ptr) {
        freeNode(ptr->leftChild);
        freeNode(ptr->rightSibling);
        delete ptr;
    }
}

template<class T, class Compare>
void PairingHeap<T, Compare>::copyFrom(const PairingHeap& other) {
    root  = copyNode(other.root, nullptr);
    count = other.count;
    cmp   = other.cmp;
}

template<class T, class Compare>
auto PairingHeap<T, Compare>::operator=(const PairingHeap& other) -> PairingHeap& {
    if (this != &other) {
        clear();
        copyFrom(other);
    }
    return *this;
}

template<class T, class Compare>
auto PairingHeap<T, Compare>::operator=(PairingHeap&& other) -> PairingHeap& {
    if (this != &other) {
        clear();
        swap(other);
    }
    return *this;
}

template<class T, class Compare>
void PairingHeap<T, Compare>::merge(PairingHeap& other) {
    // Some edge cases are handled way earlier
    if (this == &other || other.empty()) {
        return;
    }
    if (empty()) {
        swap(other);
        return;
    }
    // For simplicity, let *this be the heap that "adopts" the other
    if (cmp(other.root->value, root->value)) {
        swap(other);
    }
    // Cache the old left child
    other.root->rightSibling = root->leftChild;
    // The left child of the root will be changed, so the old one has to know
    if (root->leftChild) {
        root->leftChild->predecessor = other.root;
    }
    // Finally, link the two root nodes
    root->leftChild = other.root;
    other.root->predecessor = root;
    count += other.count;
    // Reset the other heap
    other.root = nullptr;
    other.count = 0;
}

template<class T, class Compare>
auto PairingHeap<T, Compare>::insert(const T& _val) -> proxy {
    // Simple: make a new heap and merge it
    node* res = new node(_val);
    PairingHeap singleton{ res, 1 };
    merge(singleton);
    return proxy{ res };
}

template<class T, class Compare>
T PairingHeap<T, Compare>::extractMin() {
    // Saving the root's value, leftChild, and total count before freeing the node
    const T result = peek();
    node* nextChild = root->leftChild;
    const size_t oldCount = count;
    delete root;
    root = nullptr;
    // The old root's children (also heaps)
    std::vector<PairingHeap> children;
    while (nextChild) {
        node* curr = nextChild;
        nextChild = nextChild->rightSibling;
        curr->rightSibling = curr->predecessor = nullptr;
        children.push_back(PairingHeap{ curr });
    }
    const size_t n = children.size();
    // First merge the children in pairs - that's where the name comes from
    if (n > 1) {
        for (size_t i = 0; i <= (n - 2 - (n % 2)); i += 2)
            children[i].merge(children[i + 1]);
    }
    // Then merge the resulting heaps from the last to the first
    if (n > 0) {
        for (size_t i = (n - 2 + (n % 2)); i > 0; i -= 2)
            children[i - 2].merge(children[i]);
        // The only heap left in the array is the one 
        swap(children[0]);
    }
    // Decrease the heap size (lost in the final swap)
    count = oldCount - 1;
    return result;
}

template<class T, class Compare>
auto PairingHeap<T, Compare>::decreaseKey(proxy pr, const T& newKey) -> proxy {
    // If the proxy does not point to a node in this heap -> undefined behaviour(!)
    // In case of invalid input
    if (!cmp(newKey, *pr)) {
        return pr;
    }
    // Update the value
    node* location = pr.ptr;
    location->value = newKey;
    // If the value is at the root (<=> no predecessor), nothing to change
    if (location == root) {
        return pr;
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
    // Keep the old size
    const size_t oldCount = count;
    // Make a singleton heap with the decreased value at the root
    // and merge it with the current heap
    PairingHeap singleton{ location };
    merge(singleton);
    // Restore the heap size
    count = oldCount;
    // A good practice is to return the same iterator (a.k.a. proxy)
    return pr;
}

template<class T, class Compare>
void PairingHeap<T, Compare>::clear() {
    freeNode(root);
    root = nullptr;
    count = 0;
}

template<class T, class Compare>
void PairingHeap<T, Compare>::swap(PairingHeap& other) noexcept {
    std::swap(root, other.root);
    std::swap(count, other.count);
}

template<class T, class Compare>
void swap(PairingHeap<T, Compare>& lhs, PairingHeap<T, Compare>& rhs) {
    lhs.swap(rhs);
}
