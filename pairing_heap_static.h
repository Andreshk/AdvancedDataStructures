#pragma once
#include <vector>
#include <cstddef>     // size_t
#include <algorithm>   // std::swap
#include <type_traits> // std::is_trivially_destructible_v

// On construction allocates continuous space for a predetermined number
// of nodes, and then all node pointers point to some node in this space.
// No memory is freed when a value is extracted - its node is only
// "isolated" and orphaned and the contained value is not destroyed.
// This is why a trivial (no side-effects) destructor is required.
template<class T>
class pairing_heap_static
{
    static_assert(std::is_trivially_destructible_v<T>,
        "PairingHeap requires the contained type to have a trivial destructor.");

    struct node
    {
        T value;
        node* leftChild;
        node* rightSibling;
        node* predecessor; // parent or left sibling
        node(const T& val) noexcept
            : value{ val }, leftChild{ nullptr }
            , rightSibling{ nullptr }, predecessor{ nullptr } {}
    };

    // The "arena" of all nodes for this heap. This vector contains
    // all heap nodes and we say the heap "resides" in the vector.
    // It will only grow, and extracting values will leave "holes"
    // inside it. All node pointers can be replaced by indices to this
    // vector, but keeping pointers is faster & more convenient.
    std::vector<node> nodes;
    // The address of the root node (equal to &nodes[i] for some i)
    node* root;
    // Number of actual, pointed-to nodes, i.e. "orphaned" nodes not included.
    size_t count;

    // Merges a heap into *this. The given heap must "reside" in the nodes vector.
    void merge(node* other) noexcept { root = merge(root, other); }
    // Merges two heaps, "residing" in the nodes vector, and returns a pointer to the new root.
    node* merge(node*, node*) noexcept;
public:
    class proxy
    {
        friend pairing_heap_static;
        node* ptr;
        proxy(node* ptr) : ptr{ ptr } {}
    public:
        const T& operator*()  const noexcept { return  ptr->value; }
        const T* operator->() const noexcept { return &ptr->value; }
        operator bool() const noexcept { return bool(ptr); }
        bool operator==(const proxy& other) const noexcept { return (ptr == other.ptr); }
        bool operator!=(const proxy& other) const noexcept { return (ptr != other.ptr); }
    };

    // Reserves space for given maximal count of nodes on construction
    // Note: this is the maximum amount of calls to insert(),
    // regardless of how many extractMin()-s are called inbetween. 
    pairing_heap_static(size_t capacity) { reset(capacity); }

    // We don't really need these. Not trivial, so cannot be defaulted.
    pairing_heap_static(const pairing_heap_static&) = delete;
    pairing_heap_static& operator=(const pairing_heap_static&) = delete;
    pairing_heap_static(pairing_heap_static&&) = delete;
    pairing_heap_static& operator=(pairing_heap_static&&) = delete;

    // Inserting a value is creating a singleton heap and merging it into the current heap
    proxy insert(const T&);

    // Standard operation; undefined behaviour for empty heaps
    const T& peek() const noexcept { return root->value; }

    // The most complex operation: removing the root and merging all of its children
    T extractMin();

    // Special (!)
    proxy decreaseKey(proxy, const T&) noexcept;

    // More standard methods
    size_t size() const noexcept { return count; }
    bool empty() const noexcept { return (size() == 0); }

    // Free all memory (!) and reinitialize as an empty heap with a new capacity.
    void reset(size_t);
};

template<class T>
auto pairing_heap_static<T>::merge(node* root1, node* root2) noexcept -> node*
{
    // Precondition: root1 may only be null if it is the root
    // of *this (i.e. *this is empty). root2 can never be null.
    if (!root1)
        return root2;
    // For simplicity, let root1 be the node that "adopts" the other node
    if (root2->value < root1->value)
        std::swap(root1, root2);
    // Cache the old left child
    root2->rightSibling = root1->leftChild;
    // The left child will be changed, so the old one (if any) has to know
    if (root1->leftChild)
        root1->leftChild->predecessor = root2;
    // Finally, link the two root nodes
    root1->leftChild = root2;
    root2->predecessor = root1;
    return root1;
}

template<class T>
auto pairing_heap_static<T>::insert(const T& val) -> proxy
{
    // Undefined behaviour if the node count, promised on construction, is surpassed.
    if (nodes.size() == nodes.capacity()) { return { nullptr }; }
    // Simple: make a new heap and merge it
    node* newNode = &nodes.emplace_back(val);
    merge(newNode);
    ++count;
    return { newNode };
}

template<class T>
T pairing_heap_static<T>::extractMin()
{
    // Saving the root's value & leftChild before "freeing" the node
    const T result = peek();
    node* nextChild = root->leftChild;
    // The node is detached from the heap, but not deallocated
    root->leftChild = root->rightSibling = nullptr;
    // The old root's children (also heaps)
    std::vector<node*> children;
    while (nextChild)
    {
        children.push_back(nextChild);
        node* curr = nextChild;
        nextChild = nextChild->rightSibling;
        curr->rightSibling = curr->predecessor = nullptr;
    }
    const size_t n = children.size();
    // First merge the children in pairs - that's where the name comes from
    if (n > 1) {
        for (size_t i = 0; i <= (n - 2 - (n % 2)); i += 2)
            children[i] = merge(children[i], children[i + 1]);
    }
    // Then merge the resulting heaps from the last to the first
    if (n > 0) {
        for (size_t i = (n - 2 + (n % 2)); i > 0; i -= 2)
            children[i - 2] = merge(children[i - 2], children[i]);
        // The only heap left in the array is the one 
        std::swap(root, children[0]);
    }
    --count;
    return result;
}

template<class T>
auto pairing_heap_static<T>::decreaseKey(proxy pr, const T& newKey) noexcept -> proxy
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
    // Isolate the current node as a root of a new heap...
    location->rightSibling = location->predecessor = nullptr;
    // ...and merge it with the current heap
    merge(location);
    // A good practice is to return the same iterator (a.k.a. proxy)
    return pr;
}

template<class T>
void pairing_heap_static<T>::reset(size_t newCap)
{
    nodes.clear();
    nodes.reserve(newCap);
    root = nullptr;
    count = 0;
}
