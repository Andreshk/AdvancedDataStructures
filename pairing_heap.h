#pragma once
#include <vector>
#include <cstddef>    // size_t
#include <algorithm>  // std::swap

template<class T>
class pairing_heap
{
    struct node
    {
        T value;
        node* leftChild;
        node* rightSibling;
        node* predecessor; // parent or left sibling
        node(const T& val, node* pred = nullptr) noexcept
            : value{ val }, leftChild{ nullptr }
            , rightSibling{ nullptr }, predecessor{ pred } {}
    };

    // Invariant: root->predecessor == root->rightSibling == nullptr
    node* root;
    size_t count;

    // Helper copying/deleting functions
    static node* copyNode(const node*, node*);
    static void freeNode(const node*) noexcept;
    void copyFrom(const pairing_heap&);

    // Constructing a heap from a root pointer (received, perhaps, from a child node)
    // without knowing the heap's size. Used for making a singleton heap.
    explicit pairing_heap(node* root, size_t count = 0) noexcept
        : root{ root }, count{ count } {}
public:
    class proxy
    {
        friend class pairing_heap<T>;
        node* ptr;

        explicit proxy(node* ptr) noexcept : ptr{ ptr } {}
    public:
        const T& operator*()  const noexcept { return  ptr->value; }
        const T* operator->() const noexcept { return &ptr->value; }
        operator bool() const noexcept { return bool(ptr); }
        bool operator==(const proxy& other) const noexcept { return ptr == other.ptr; }
        bool operator!=(const proxy& other) const noexcept { return ptr != other.ptr; }
    };

    // Standard big 6
    pairing_heap() noexcept : pairing_heap{ nullptr, 0 } {}
    pairing_heap(const pairing_heap&);
    pairing_heap& operator=(const pairing_heap&);
    pairing_heap(pairing_heap&&) noexcept;
    pairing_heap& operator=(pairing_heap&&) noexcept;
    ~pairing_heap() { clear(); }

    // x.merge(y) always leaves y empty (!) regardless which root value
    // is smaller. Afterwards x contains all values of both x and y.
    void merge(pairing_heap&) noexcept;

    // Inserting a value - creates a singleton heap and merges with the current heap
    proxy insert(const T&);

    // Standard operation; undefined behaviour for empty heaps
    const T& peek() const noexcept { return root->value; }

    // The most complex operation: removing the root and merging all of its children
    T extractMin();

    // Special (!)
    proxy decreaseKey(proxy, const T&) noexcept;

    // More standard methods
    size_t size() const noexcept { return count; }
    bool empty() const noexcept { return (root == nullptr); }
    void clear() noexcept;

    // For convenience
    void swap(pairing_heap&) noexcept;
};

template<class T>
auto pairing_heap<T>::copyNode(const node* ptr, node* _pred) -> node*
{
    if (!ptr)
        return nullptr;
    node* tmp = new node(ptr->value, _pred);
    tmp->leftChild = copyNode(ptr->leftChild, tmp);
    tmp->rightSibling = copyNode(ptr->rightSibling, tmp);
    return tmp;
}

template<class T>
void pairing_heap<T>::freeNode(const node* ptr) noexcept
{
    if (ptr) {
        freeNode(ptr->leftChild);
        freeNode(ptr->rightSibling);
        delete ptr;
    }
}

template<class T>
void pairing_heap<T>::copyFrom(const pairing_heap& other)
{
    root = copyNode(other.root, nullptr);
    count = other.count;
}

template<class T>
pairing_heap<T>::pairing_heap(const pairing_heap& other)
{
    copyFrom(other);
}

template<class T>
pairing_heap<T>& pairing_heap<T>::operator=(const pairing_heap& other)
{
    if (this != &other) {
        clear();
        copyFrom(other);
    }
    return *this;
}

template<class T>
pairing_heap<T>::pairing_heap(pairing_heap&& other) noexcept : pairing_heap()
{
    swap(other);
}

template<class T>
pairing_heap<T>& pairing_heap<T>::operator=(pairing_heap&& other) noexcept
{
    if (this != &other) {
        clear();
        swap(other);
    }
    return *this;
}

template<class T>
void pairing_heap<T>::merge(pairing_heap<T>& other) noexcept
{
    // Some edge cases are handled way earlier
    if (this == &other || other.empty())
        return;
    if (empty()) {
        swap(other);
        return;
    }
    // For simplicity, let *this be the heap that "adopts" the other
    if (other.root->value < root->value)
        swap(other);
    // Cache the old left child
    other.root->rightSibling = root->leftChild;
    // The left child of the root will be changed, so the old one has to know
    if (root->leftChild)
        root->leftChild->predecessor = other.root;
    // Finally, link the two root nodes
    root->leftChild = other.root;
    other.root->predecessor = root;
    count += other.count;
    // Reset the other heap
    other.root = nullptr;
    other.count = 0;
}

template<class T>
auto pairing_heap<T>::insert(const T& _val) -> proxy
{
    // Simple: make a new heap and merge it
    node* res = new node(_val);
    merge(pairing_heap{ res, 1 });
    return proxy{ res };
}

template<class T>
T pairing_heap<T>::extractMin()
{
    // Saving the root's value, leftChild, and total count before freeing the node
    const T result = peek();
    node* nextChild = root->leftChild;
    const size_t oldCount = count;
    delete root;
    root = nullptr;
    // The old root's children (also heaps)
    std::vector<pairing_heap> children;
    while (nextChild)
    {
        node* curr = nextChild;
        nextChild = nextChild->rightSibling;
        curr->rightSibling = curr->predecessor = nullptr;
        children.push_back(pairing_heap{ curr });
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

template<class T>
auto pairing_heap<T>::decreaseKey(proxy pr, const T& newKey) noexcept -> proxy
{
    // If the proxy does not point to a node in this heap -> undefined behaviour(!)
    // In case of invalid input
    if (!(newKey < *pr))
        return pr;
    // Update the value
    node* location = pr.ptr;
    location->value = newKey;
    // If the value is at the root (<=> no predecessor), nothing to change
    if (location == root)
        return pr;
    // Tell its left sibling/parent it has a new right sibling/left child
    if (location == location->predecessor->rightSibling)
        location->predecessor->rightSibling = location->rightSibling;
    else
        location->predecessor->leftChild = location->rightSibling;
    // Tell its right sibling (if any) it has a new left sibling
    if (location->rightSibling)
        location->rightSibling->predecessor = location->predecessor;
    // Isolate the current node as a root of a new heap
    location->rightSibling = location->predecessor = nullptr;
    // Keep the old size
    const size_t oldCount = count;
    // Make a singleton heap with the decreased value at the root
    // and merge it with the current heap
    merge(pairing_heap{ location });
    // Restore the heap size
    count = oldCount;
    // A good practice is to return the same iterator (a.k.a. proxy)
    return pr;
}

template<class T>
void pairing_heap<T>::clear() noexcept
{
    freeNode(root);
    root = nullptr;
    count = 0;
}

template<class T>
void pairing_heap<T>::swap(pairing_heap& other) noexcept
{
    std::swap(root, other.root);
    std::swap(count, other.count);
}

template<class T>
void swap(pairing_heap<T>& lhs, pairing_heap<T>& rhs) noexcept
{
    lhs.swap(rhs);
}
