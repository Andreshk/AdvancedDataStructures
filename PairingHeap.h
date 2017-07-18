#pragma once
#include <vector>
#include <cstddef>   // size_t
#include <algorithm> // std::swap

template<class T>
class PairingHeap
{
    struct Node
    {
        T value;
        Node* leftChild;
        Node* rightSibling;
        Node* predecessor;
        Node(const T& _val) noexcept : value(_val), leftChild(nullptr),
            rightSibling(nullptr), predecessor(nullptr) {}
    };

    Node* root;
    size_t count;

    // helper copying/deleting functions
    static Node* copyNode(const Node*, Node*);
    static void freeNode(const Node*) noexcept;
    void copyFrom(const PairingHeap&);

    // Constructing a heap by its root (received, perhaps, from a child node)
    // without knowing the child's size. Used for making a singleton heap.
    explicit PairingHeap(Node*, size_t = 0) noexcept;
public:
    class proxy
    {
        friend class PairingHeap<T>;
        Node* ptr;

        explicit proxy(Node*) noexcept;
    public:
        const T& operator*() const noexcept;
        const T* operator->() const noexcept;
        operator bool() const noexcept;
        friend bool operator==(const proxy& lhs, const proxy& rhs) noexcept { return lhs.ptr == rhs.ptr; }
        friend bool operator!=(const proxy& lhs, const proxy& rhs) noexcept { return !(lhs == rhs); }
    };

    // standard big 6
    PairingHeap() noexcept;
    PairingHeap(const PairingHeap&);
    PairingHeap& operator=(const PairingHeap&);
    PairingHeap(PairingHeap&&) noexcept;
    PairingHeap& operator=(PairingHeap&&) noexcept;
    ~PairingHeap();

    // x.merge(y) always leaves the heap y empty (!) regardless which
    // root value is smaller; afterwards x contains all values of both x and y.
    void merge(PairingHeap&) noexcept;

    // inserting a value - creates a singleton heap and merges with the current heap
    proxy insert(const T&);

    // standard operation; undefined behaviour for empty heaps
    const T& peek() const noexcept;

    // the most complex operation: removing the root and merging all of its children
    T extractMin();

    // special (!)
    proxy decreaseKey(proxy, const T&) noexcept;

    // more standard methods
    size_t size() const noexcept;
    bool empty() const noexcept;
    // clear() clears all allocated memory and returns the heap to a valid, empty state
    void clear() noexcept;

    // for convenience
    friend void swap(PairingHeap& lhs, PairingHeap& rhs) noexcept
    {
        std::swap(lhs.root, rhs.root);
        std::swap(lhs.count, rhs.count);
    }
};

template<class T>
PairingHeap<T>::proxy::proxy(Node* _ptr) noexcept : ptr(_ptr) {}

template<class T>
const T& PairingHeap<T>::proxy::operator*() const noexcept
{
    return ptr->value;
}

template<class T>
const T* PairingHeap<T>::proxy::operator->() const noexcept
{
    return &ptr->value;
}

template<class T>
PairingHeap<T>::proxy::operator bool() const noexcept
{
    return bool(ptr);
}

template<class T>
auto PairingHeap<T>::copyNode(const Node* ptr, Node* _pred) -> Node*
{
    if (!ptr)
        return nullptr;
    Node* tmp = new Node(ptr->value);
    tmp->leftChild = copyNode(ptr->leftChild, tmp);
    tmp->rightSibling = copyNode(ptr->rightSibling, tmp);
    tmp->predecessor = _pred;
    return tmp;
}

template<class T>
void PairingHeap<T>::freeNode(const Node* ptr) noexcept
{
    if (ptr)
    {
        freeNode(ptr->leftChild);
        freeNode(ptr->rightSibling);
        delete ptr;
    }
}

template<class T>
void PairingHeap<T>::copyFrom(const PairingHeap& other)
{
    root = copyNode(other.root, nullptr);
    count = other.count;
}

template<class T>
PairingHeap<T>::PairingHeap(Node* _root, size_t _count) noexcept : root(_root), count(_count) {}

template<class T>
PairingHeap<T>::PairingHeap() noexcept : PairingHeap(nullptr, 0) {}

template<class T>
PairingHeap<T>::PairingHeap(const PairingHeap& other)
{
    copyFrom(other);
}

template<class T>
PairingHeap<T>& PairingHeap<T>::operator=(const PairingHeap& other)
{
    if (this != &other)
    {
        clear();
        copyFrom(other);
    }
    return *this;
}

template<class T>
PairingHeap<T>::PairingHeap(PairingHeap&& other) noexcept : PairingHeap()
{
    swap(*this, other);
}

template<class T>
PairingHeap<T>& PairingHeap<T>::operator=(PairingHeap&& other) noexcept
{
    if (this != &other)
    {
        clear();
        swap(*this, other);
    }
    return *this;
}

template<class T>
PairingHeap<T>::~PairingHeap()
{
    clear();
}

template<class T>
void PairingHeap<T>::merge(PairingHeap<T>& other) noexcept
{
    if (this == &other || other.empty())
        return;
    if (empty())
    {
        swap(*this, other);
        return;
    }
    if (other.root->value < root->value)
        swap(*this, other);
    other.root->rightSibling = root->leftChild;
    if (root->leftChild)
        root->leftChild->predecessor = other.root;
    root->leftChild = other.root;
    root->leftChild->predecessor = root;
    count += other.count;
    other.root = nullptr;
    other.count = 0;
}

template<class T>
auto PairingHeap<T>::insert(const T& _val) -> proxy
{
    // simple
    Node* res = new Node(_val);
    merge(PairingHeap(res, 1));
    return proxy(res);
}

template<class T>
const T& PairingHeap<T>::peek() const noexcept
{
    return root->value;
}

template<class T>
T PairingHeap<T>::extractMin()
{
    // saving the root's value and leftChild before freeing the node
    const T result = peek();
    Node* nextChild = root->leftChild;
    delete root;
    root = nullptr;
    // also saved for later
    const size_t oldCount = count;
    // the old root's children in an array (also heaps)
    std::vector<PairingHeap> children;
    while (nextChild)
    {
        Node* curr = nextChild;
        nextChild = nextChild->rightSibling;
        curr->rightSibling = curr->predecessor = nullptr;
        children.push_back(std::move(PairingHeap(curr)));
    }
    const size_t n = children.size();
    // first merge the children in pairs - that's where the name comes from
    if (n > 1)
        for (size_t i = 0; i <= (n - 2 - (n % 2)); i += 2)
            children[i].merge(children[i + 1]);
    // then merge the resulting heaps from the last to the first
    if (n > 0)
    {
        for (size_t i = (n - 2 + (n % 2)); i > 0; i -= 2)
            children[i - 2].merge(children[i]);
        // finally, merge the only heap left with the current one
        swap(*this, children[0]);
    }
    // decreasing the heap size (lost in the final swap)
    count = oldCount - 1;
    return result;
}

template<class T>
auto PairingHeap<T>::decreaseKey(proxy pr, const T& newKey) noexcept -> proxy
{
    // if the proxy does not point to a node in this heap -> undefined behaviour(!)
    // in case of invalid input
    if (!(newKey < *pr))
        return pr;
    // updating the value
    Node* location = pr.ptr;
    location->value = newKey;
    // if the value is at the root (<=> no left child), don't change anything
    if (location == root)
        return pr;
    // tell its left sibling/predecessor it has a new right sibling/successor
    if (location == location->predecessor->rightSibling)
        location->predecessor->rightSibling = location->rightSibling;
    else
        location->predecessor->leftChild = location->rightSibling;
    // tell its right sibling (if any) it has a new left sibling
    if (location->rightSibling)
        location->rightSibling->predecessor = location->predecessor;
    // isolate the current node as a root of a new heap
    location->rightSibling = location->predecessor = nullptr;
    // keep the old size
    const size_t oldCount = count;
    // make a separate heap with the decreased value at the root
    // and merge it with the current heap
    merge(PairingHeap(location));
    // restore the heap size
    count = oldCount;
    // good practises - return the same iterator (a.k.a. proxy)
    return pr;
}

template<class T>
size_t PairingHeap<T>::size() const noexcept
{
    return count;
}

template<class T>
bool PairingHeap<T>::empty() const noexcept
{
    return (root == nullptr);
}

template<class T>
void PairingHeap<T>::clear() noexcept
{
    freeNode(root);
    root = nullptr;
    count = 0;
}
