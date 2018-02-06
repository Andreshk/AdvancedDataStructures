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
        node* predecessor;
        node(const T& _val, node* _pred = nullptr) noexcept : value(_val), leftChild(nullptr),
            rightSibling(nullptr), predecessor(_pred) {}
    };

    node* root;
    size_t count;

    // helper copying/deleting functions
    static node* copyNode(const node*, node*);
    static void freeNode(const node*) noexcept;
    void copyFrom(const pairing_heap&);

    // Constructing a heap by its root (received, perhaps, from a child node)
    // without knowing the child's size. Used for making a singleton heap.
    explicit pairing_heap(node*, size_t = 0) noexcept;
public:
    class proxy
    {
        friend class pairing_heap<T>;
        node* ptr;

        explicit proxy(node*) noexcept;
    public:
        const T& operator*() const noexcept;
        const T* operator->() const noexcept;
        operator bool() const noexcept;
        bool operator==(const proxy&) const noexcept;
        bool operator!=(const proxy&) const noexcept;
    };

    // standard big 6
    pairing_heap() noexcept;
    pairing_heap(const pairing_heap&);
    pairing_heap& operator=(const pairing_heap&);
    pairing_heap(pairing_heap&&) noexcept;
    pairing_heap& operator=(pairing_heap&&) noexcept;
    ~pairing_heap();

    // x.merge(y) always leaves the heap y empty (!) regardless which
    // root value is smaller; afterwards x contains all values of both x and y.
    void merge(pairing_heap&) noexcept;

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
    void swap(pairing_heap&) noexcept;
};

template<class T>
pairing_heap<T>::proxy::proxy(node* _ptr) noexcept : ptr(_ptr) {}

template<class T>
const T& pairing_heap<T>::proxy::operator*() const noexcept
{
    return ptr->value;
}

template<class T>
const T* pairing_heap<T>::proxy::operator->() const noexcept
{
    return &ptr->value;
}

template<class T>
pairing_heap<T>::proxy::operator bool() const noexcept
{
    return bool(ptr);
}

template<class T>
bool pairing_heap<T>::proxy::operator==(const proxy& other) const noexcept
{
    return ptr = other.ptr;
}

template<class T>
bool pairing_heap<T>::proxy::operator!=(const proxy& other) const noexcept
{
    return !(*this == other);
}

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
    if (ptr)
    {
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
pairing_heap<T>::pairing_heap(node* _root, size_t _count) noexcept : root(_root), count(_count) {}

template<class T>
pairing_heap<T>::pairing_heap() noexcept : pairing_heap(nullptr, 0) {}

template<class T>
pairing_heap<T>::pairing_heap(const pairing_heap& other)
{
    copyFrom(other);
}

template<class T>
pairing_heap<T>& pairing_heap<T>::operator=(const pairing_heap& other)
{
    if (this != &other)
    {
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
    if (this != &other)
    {
        clear();
        swap(other);
    }
    return *this;
}

template<class T>
pairing_heap<T>::~pairing_heap()
{
    clear();
}

template<class T>
void pairing_heap<T>::merge(pairing_heap<T>& other) noexcept
{
    if (this == &other || other.empty())
        return;
    if (empty())
    {
        swap(other);
        return;
    }
    if (other.root->value < root->value)
        swap(other);
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
auto pairing_heap<T>::insert(const T& _val) -> proxy
{
    // simple
    node* res = new node(_val);
    merge(pairing_heap(res, 1));
    return proxy(res);
}

template<class T>
const T& pairing_heap<T>::peek() const noexcept
{
    return root->value;
}

template<class T>
T pairing_heap<T>::extractMin()
{
    // saving the root's value and leftChild before freeing the node
    const T result = peek();
    node* nextChild = root->leftChild;
    delete root;
    root = nullptr;
    // also saved for later
    const size_t oldCount = count;
    // the old root's children in an array (also heaps)
    std::vector<pairing_heap> children;
    while (nextChild)
    {
        node* curr = nextChild;
        nextChild = nextChild->rightSibling;
        curr->rightSibling = curr->predecessor = nullptr;
        children.push_back(std::move(pairing_heap(curr)));
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
        swap(children[0]);
    }
    // decreasing the heap size (lost in the final swap)
    count = oldCount - 1;
    return result;
}

template<class T>
auto pairing_heap<T>::decreaseKey(proxy pr, const T& newKey) noexcept -> proxy
{
    // if the proxy does not point to a node in this heap -> undefined behaviour(!)
    // in case of invalid input
    if (!(newKey < *pr))
        return pr;
    // updating the value
    node* location = pr.ptr;
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
    merge(pairing_heap(location));
    // restore the heap size
    count = oldCount;
    // good practises - return the same iterator (a.k.a. proxy)
    return pr;
}

template<class T>
size_t pairing_heap<T>::size() const noexcept
{
    return count;
}

template<class T>
bool pairing_heap<T>::empty() const noexcept
{
    return (root == nullptr);
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
