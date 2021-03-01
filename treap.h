#pragma once
#include <cstddef>    // size_t
#include <random>
#include <functional> // std::less
#include <type_traits>
#include "vassert.h"

// Represents node priorities. Can be any ordered type,
// but is typically a floating-point number in [0;1)
using Real = float;

// Forms the tree skeleton & is used to abstract away tree operations
// that do not depend on the values in the tree, f.e. rotations.
// Each treap also contains a sentinel node of this type for the end() iterator to point to.
struct NodeBase {
    NodeBase* parent;
    NodeBase* left;
    NodeBase* right;
    Real pr;
    NodeBase(NodeBase* parent, Real pr)
        : parent{ parent }, left{ nullptr }, right{ nullptr }, pr{ pr } {};
    // Moves a pointer to the node, containing the next value in the tree
    static const NodeBase* advance(const NodeBase*) noexcept;
    // Moves a pointer to the node, containing the previous value in the tree
    static const NodeBase* regress(const NodeBase*) noexcept;
    // Updates a node's parent for later deletion of this node
    static void updateParent(const NodeBase*, NodeBase*) noexcept;
    // Left & right tree rotations
    static void rotateLeft(NodeBase*&) noexcept;
    static void rotateRight(NodeBase*&) noexcept;
    // Checks whether the heap property holds for a given tree
    static bool validHeap(const NodeBase*) noexcept;
    // Bubbles up a node to restore the heap property after insertion
    static void bubbleUp(NodeBase*) noexcept;
    // Bubbles down a node & removes it from the tree for deletion
    static void bubbleDown(const NodeBase*) noexcept;
};

// A node with a value of type T.
template <typename T>
struct Node : NodeBase {
    T value;
    template<class... Args> // only constructor you need, lol
    Node(NodeBase* parent, Real pr, Args&&... args)
        : NodeBase{ parent, pr }, value(std::forward<Args>(args)...) {}
    // Performs a deep copy of a tree, pointer by a given root pointer
    // and sets the copy's parent pointer to the given value.
    static Node* copyNode(const Node*, const NodeBase*);
};

template<typename T, typename Comp = std::less<T>>
class Treap {
    /* Each treap contains a dummy (sentinel) node that the end iterator
       points to. The following invariants hold for this node:
       - dummy.parent == &dummy - the dummy node is its own parent and is the only node with this property
       - dummy.left points to the actual root node of the tree
       - dummy.right should (and will) always be null in order for ++(--end()) == end()
       Note: parent pointers will never be null - saves a LOT of runtime checks.
       Also ptr->parent == ptr only for a pointer to the dummy node - this is
       a beautiful way to check if an iterator == end() without calling end().
       Finally, normally --begin() is undefined behaviour - here as a bonus --begin() == end() */
    NodeBase dummy;
    size_t count;
    Comp comp;

    // For easier access to the actual root of the tree structure
          NodeBase*& root()       noexcept { return dummy.left; }
    NodeBase* const& root() const noexcept { return dummy.left; }
    // Provide safe & convenient downcasting from a Base node to a Node with a value.
    static Node<T>* toNode(NodeBase* ptr) {
        vassert(!(ptr && ptr->parent == ptr) && "Forbidden reinterpret_cast on dummy node!");
        return reinterpret_cast<Node<T>*>(ptr);
    }
    static const Node<T>* toNode(const NodeBase* ptr) {
        vassert(!(ptr && ptr->parent == ptr) && "Forbidden reinterpret_cast on dummy node!");
        return reinterpret_cast<const Node<T>*>(ptr);
    }
    // Helper methods for copying, searching & deleting nodes
    void copyFrom(const Treap&);
    const Node<T>* findNode(const T&) const;
    static void freeNode(const NodeBase*) noexcept;
    // Checks whether the BST property holds for a given tree,
    // knowing the range, in which all values should belong.
    bool validBST(const Node<T>*, const T&, const T&) const noexcept;
public:
    // Standard bidirectional read-only iterator
    class iterator {
        friend class Treap<T, Comp>;
        const NodeBase* ptr;
        // Only the treap can construct iterators to itself
        iterator(const NodeBase* ptr) noexcept : ptr{ ptr } { vassert(ptr); }
    public:
        // to-do: all typedefs & operators to qualify as bidirectional
        // iterator (or whatever category std::set<T>::iterator is)
        const T& operator*() const noexcept { return toNode(ptr)->value; }
        const T* operator->() const noexcept { return &toNode(ptr)->value; }
        iterator& operator++() noexcept { ptr = NodeBase::advance(ptr); return *this; }
        iterator& operator--() noexcept { ptr = NodeBase::regress(ptr); return *this; }
        iterator operator++(int) noexcept { auto copy = *this; ++(*this); return copy; }
        iterator operator--(int) noexcept { auto copy = *this; --(*this); return copy; }
        explicit operator bool() const noexcept { return (ptr->parent != ptr); } // not standard in STL
        auto operator<=>(const iterator&) const noexcept = default;
    };
    // to-do: why does this cause internal compiler errors :/
    Treap(/*const Comp& comp = Comp{}*/) noexcept(std::is_nothrow_copy_constructible_v<Comp>)
        : dummy{ &dummy, Real{ 0 } }, count{ 0 }, comp{ /*comp*/ } {}
    Treap(const Treap&);
    Treap& operator=(const Treap&);
    Treap(Treap&&) noexcept;
    Treap& operator=(Treap&&) noexcept;
    ~Treap() { clear(); }

    // Notes about insertion interface:
    // - sizeof(mt19937_64) == 5000 => maybe not feasible to keep a copy inside each Treap
    // - having a static mt19937_64 per Treap type will lead to race conditions when using Treap from different threads :(
    // - allocators return shlightly random addresses -> maybe use them (& remove the priority fields)
    // - maybe this is the way to go, and forbid adding more than 1 value at a time

    // The following three functions insert a single value with its corresponding random value in [0;1).
    // An iterator to the inserted value is returned.
    iterator insert(Real pr, const T& value) { return emplace(pr, value); }
    iterator insert(Real pr, T&& value) { return emplace(pr, std::move(value)); }
    template<class... Args>
    iterator emplace(Real, Args&&...);
    //template<class InputIt> // to-do: constrain w/ a concept
    //void insert(InputIt, InputIt);
    //void insert(std::initializer_list<T> il) { insert(il.begin(), il.end()); } // to-do: use any forward range (!)
    
    // Erases a value & returns an iterator to the next one
    iterator erase(iterator);
    // Erases a range of values & returns an iterator to the one after the last deleted value
    iterator erase(iterator, iterator);
    // Searches for a value & returns an iterator to it on success (end() otherwise)
    iterator find(const T&) const;

    // Returns an iterator to the first (smallest) value in the treap
    iterator begin() const noexcept;
    // Returns an iterator to one-past-the-last (largest) value in the treap
    iterator end() const noexcept;
    // Returns the numebr of values in the treap
    size_t size() const noexcept { return count; }
    // Checks whetehr the treap is empty
    bool empty() const noexcept { return (count == 0); }
    // Removes all values from the treap, deallocates all memory & returns it to a default-constructed state.
    void clear() noexcept;
    // Swaps the treap's contents with another one
    void swap(Treap&) noexcept;
};

// NodeBase methods, i.e. basic tree operations that
// do not depend on the values contained within
const NodeBase* NodeBase::advance(const NodeBase* ptr) noexcept {
    if (ptr->right) { // find the leftmost right ancestor (if there are any)
        ptr = ptr->right;
        while (ptr->left) {
            ptr = ptr->left;
        }
    } else { // otherwise, find the smallest right predecessor by going as up-left as possible
        // parent pointers will never be nullptr!
        NodeBase* parent = ptr->parent;
        while (/*ptr->parent &&*/ptr == parent->right) {
            ptr = parent;
            parent = parent->parent;
        }
        ptr = parent; // this is a step up-right
    }
    return ptr;
}

const NodeBase* NodeBase::regress(const NodeBase* ptr) noexcept {
    // symmetrical procedure for decrementing an iterator
    if (ptr->left) {
        ptr = ptr->left;
        while (ptr->right) {
            ptr = ptr->right;
        }
    } else {
        NodeBase* parent = ptr->parent;
        while (ptr == parent->left) {
            ptr = parent;
            parent = parent->parent;
        }
        ptr = parent;
    }
    return ptr;
}

void NodeBase::updateParent(const NodeBase* location, NodeBase* newChild) noexcept {
    // "Removes" a node from the tree by replacing its
    // parent pointer to itself with a new child node.
    // Reminder that parent pointers are always non-null
    NodeBase* parent = location->parent;
    if (location == parent->left) {
        parent->left = newChild;
    } else {
        parent->right = newChild;
    }
    if (newChild) {
        newChild->parent = parent;
    }
}

void NodeBase::rotateLeft(NodeBase*& ptr) noexcept {
    // if the tree is empty or has no right subtree, rotating left is impossible
    vassert(ptr && ptr->right);
    NodeBase* tmp = ptr->right;
    ptr->right = tmp->left;
    if (ptr->right) {
        ptr->right->parent = ptr;
    }
    tmp->parent = ptr->parent;
    tmp->left = ptr;
    ptr->parent = tmp;
    ptr = tmp;
}

void NodeBase::rotateRight(NodeBase*& ptr) noexcept {
    // if the tree is empty or has no left subtree, rotating right is impossible
    vassert(ptr && ptr->left);
    NodeBase* tmp = ptr->left;
    ptr->left = tmp->right;
    if (ptr->left) {
        ptr->left->parent = ptr;
    }
    tmp->parent = ptr->parent;
    tmp->right = ptr;
    ptr->parent = tmp;
    ptr = tmp;
}

bool NodeBase::validHeap(const NodeBase* ptr) noexcept {
    return ((!ptr->left || (ptr->pr <= ptr->left->pr && validHeap(ptr->left)))
        && (!ptr->right || (ptr->pr <= ptr->right->pr && validHeap(ptr->right))));
}

void NodeBase::bubbleUp(NodeBase* ptr) noexcept {
    NodeBase* parent = ptr->parent;
    // Bubble up the newly inserted note via rotations until
    // the heap property is restored (or it reaches the root)
    while (parent != parent->parent && ptr->pr < parent->pr) {
        // Parent node is a valid, "full" one => rotate the new node up to its position
        NodeBase* grandParent = parent->parent;
        NodeBase*& toParent = (grandParent->left == parent ? grandParent->left : grandParent->right);
        if (parent->left == ptr) { // left child -> rotate right
            NodeBase::rotateRight(toParent);
            vassert(parent == ptr->right);
        } else { // right child -> rotate left
            NodeBase::rotateLeft(toParent);
            vassert(parent == ptr->left);
        }
        vassert(toParent == ptr && grandParent == ptr->parent);
        parent = grandParent;
    }
}

void NodeBase::bubbleDown(const NodeBase* ptr) noexcept {
    // Bubble down the node to be deleted to a leaf & cut this leaf
    while (ptr->left && ptr->right) {
        NodeBase* parent = ptr->parent;
        NodeBase*& toLocation = (ptr == parent->left ? parent->left : parent->right);
        // The child with the smallest priority should take its parent's place
        if (ptr->left->pr < ptr->right->pr) {
            // The left child should be above the right
            NodeBase::rotateRight(toLocation);
        } else {
            NodeBase::rotateLeft(toLocation);
        }
    }
    // If the node has one child, it's 1 rotation away from becoming
    // a leaf, so we directly link its parent to this child.
    if (ptr->left) {
        NodeBase::updateParent(ptr, ptr->left);
    } else if (ptr->right) {
        NodeBase::updateParent(ptr, ptr->right);
    } else {
        // No children => leaf node => cut it & let its parent know
        NodeBase::updateParent(ptr, nullptr);
    }
}

template<typename T>
Node<T>* Node<T>::copyNode(const Node<T>* ptr, const NodeBase* parent) {
    if (!ptr) {
        return nullptr;
    }
    Node* res = new Node(parent, ptr->pr, ptr->value);
    res->left = copyNode(ptr->left, res);
    res->right = copyNode(ptr->right, res);
    return res;
}

// Treap methods
template<typename T, typename Comp>
void Treap<T, Comp>::copyFrom(const Treap& other) {
    vassert(!root());
    root() = Node<T>::copyNode(other.root(), &dummy);
    count = other.count;
    comp = other.comp;
}

template<typename T, typename Comp>
auto Treap<T, Comp>::findNode(const T& value) const -> const Node<T>* {
    const Node<T>* location = toNode(root());
    while (location) {
        if (comp(value, location->value)) {
            location = toNode(location->left);
        } else if (comp(location->value, value)) {
            location = toNode(location->right);
        } else {
            return location; //found it
        }
    }
    return nullptr; // not found
}

template<typename T, typename Comp>
void Treap<T, Comp>::freeNode(const NodeBase* ptr) noexcept {
    if (ptr) {
        freeNode(ptr->left);
        freeNode(ptr->right);
        delete toNode(ptr);
    }
}

template <typename T, typename Comp>
bool Treap<T, Comp>::validBST(const Node<T>* ptr, const T& min, const T& max) const noexcept {
    return (!comp(ptr->value, min) && !comp(max, ptr->value)
        && (!ptr->left || validBST(toNode(ptr->left), min, ptr->value))
        && (!ptr->right || validBST(toNode(ptr->right), ptr->value, max)));
}

template<typename T, typename Comp>
Treap<T, Comp>::Treap(const Treap& other) : Treap() {
    copyFrom(other);
    vassert(dummy.parent == &dummy);
}

template<typename T, typename Comp>
auto Treap<T, Comp>::operator=(const Treap& other) -> Treap& {
    if (this != &other) {
        clear();
        copyFrom(other);
    }
    vassert(dummy.parent == &dummy);
    return *this;
}

template<typename T, typename Comp>
Treap<T, Comp>::Treap(Treap&& other) noexcept : Treap() {
    swap(other);
}

template<typename T, typename Comp>
auto Treap<T, Comp>::operator=(Treap&& other) noexcept -> Treap& {
    if (this != &other) {
        clear();
        swap(other);
    }
    return *this;
}

template<typename T, typename Comp>
template<class ...Args>
auto Treap<T, Comp>::emplace(Real pr, Args&&... _args) -> iterator {
    Node<T>* newNode = new Node<T>(nullptr, pr, std::forward<Args>(_args)...);
    const T& newVal = newNode->value;
    Node<T>* location = toNode(root());
    NodeBase* parent = &dummy; // <=> root()->parent
    bool isLeft = true; // Whether location is the left or right child of its root
    // Standard BST insertion
    while (location) {
        parent = location;
        if (comp(newVal, location->value)) {
            location = toNode(location->left);
            isLeft = true;
        } else {
            location = toNode(location->right);
            isLeft = false;
        }
    }
    location = newNode;
    location->parent = parent;
    if (isLeft) {
        parent->left = location;
    } else {
        parent->right = location;
    }
    // Bubble up, using the parent pointers
    NodeBase::bubbleUp(location);
    ++count;
    vassert(NodeBase::validHeap(root()));
    vassert(validBST(toNode(root()), *begin(), *--end()));
    return { location };
}

/*template<typename T, typename Comp>
template<class InputIt>
void Treap<T, Comp>::insert(InputIt from, InputIt to) {
    while (from != to) {
        insert(*from);
        ++from;
    }
}*/

// Note: this method does not depend on Comp, but is
// too much of a hassle to lift it outside of the Treap.
template<typename T, typename Comp>
auto Treap<T, Comp>::erase(iterator it) -> iterator {
    // Another bonus: in STL erase(end()) is undefined behaviour,
    // here it's perfectly fine & returns end() immediately.
    if (it == end()) {
        return it;
    }
    const NodeBase* location = it.ptr;
    ++it;
    --count;
    // Deletion is actually much easier (although slower) than
    // in a BST - we just use rotations to bubble down to a leaf.
    NodeBase::bubbleDown(location);
    // Free the memory in question & return the updated iterator
    delete toNode(location);
    if (root()) {
        vassert(NodeBase::validHeap(root()));
        vassert(validBST(toNode(root()), *begin(), *--end()));
    }
    return it;
}

template<typename T, typename Comp>
auto Treap<T, Comp>::erase(iterator from, iterator to) -> iterator {
    while (from != to) {
        from = erase(from);
    }
    return to; // to-do: what is the point of this?
}

template<typename T, typename Comp>
auto Treap<T, Comp>::find(const T& value) const -> iterator {
    const Node<T>* location = findNode(value);
    return (location ? iterator(location) : end());
}

template<typename T, typename Comp>
auto Treap<T, Comp>::begin() const noexcept -> iterator {
    // Note: don't assign to root(); this should be ==end() for empty treaps
    const NodeBase* location = &dummy;
    while (location->left) {
        location = location->left;
    }
    return { location };
}

template<typename T, typename Comp>
auto Treap<T, Comp>::end() const noexcept -> iterator {
    return { &dummy };
}

template<typename T, typename Comp>
void Treap<T, Comp>::clear() noexcept {
    freeNode(toNode(root()));
    root() = nullptr;
    count = 0;
}

template<typename T, typename Comp>
void Treap<T, Comp>::swap(Treap& other) noexcept {
    using std::swap;
    swap(root(), other.root());
    swap(count, other.count);
    swap(comp, other.comp);
    vassert(dummy.parent == &dummy);
    vassert(other.dummy.parent == &other.dummy);
}

template<typename T, typename Comp>
void swap(Treap<T, Comp>& lhs, Treap<T, Comp>& rhs) noexcept {
    lhs.swap(rhs);
}
