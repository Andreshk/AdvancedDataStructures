#pragma once
#include <cstddef>    // size_t
#include <random>
#include <functional> // std::less
#include <type_traits>
#include "vassert.h"

using Real = float;

template<typename T, typename Comp = std::less<T>>
class Treap {
    struct Node;
    struct NodeBase {
        // to-do: these SHOULD be NodeBase => lift outside of the Treap class
        // also lift Node<T> outside, so that it odesn't depend on Comp
        Node* parent;
        Node* left;
        Node* right;
        NodeBase(Node* parent) : parent{ parent }, left{ nullptr }, right{ nullptr } {}
    };
    struct Node : NodeBase {
        T value;
        Real pr;
        template<class... Args> // only constructor you need, lol
        Node(Node* parent, Real pr, Args&&... args)
            : NodeBase{ parent }, value(std::forward<Args>(args)...), pr{ pr } {}
    };
    /* dummy node, for which the following hold:
     - dummy.parent == &dummy, i.e. the dummy node is its own parent
     - dummy.left   points to the actual root node of the tree. see root() below
     - dummy.right  should (and will) always be null in order for ++(--end()) == end() */
    NodeBase dummy;
    /* Note: parent pointers will never be null - saves a LOT of runtime checks. Also ptr->parent == ptr
       only for a pointer to the dummy node - this is a beautiful way to check if an iterator == end().
       Finally, normally --begin() is undefined behaviour - here as a bonus --begin() == end() */
    size_t count;
    Comp comp;

    // _this_as_node()         <=> &dummy, from which we have
    // _this_as_node()->left   <=> dummy.left <=> root() and
    // _this_as_node()->parent <=> dummy.parent <=> &dummy <=> _this_as_node()
    // _this_as_node()         <=> root()->parent as long as root()!=nullptr
          Node*& root()       noexcept { return dummy.left; } // <=> _this_as_node()->left
    Node* const& root() const noexcept { return dummy.left; }
    // to-do: avoid these (!)
          Node* _this_as_node()       noexcept { return reinterpret_cast<      Node*>(&dummy); }
    const Node* _this_as_node() const noexcept { return reinterpret_cast<const Node*>(&dummy); }

    void copyFrom(const Treap&);
    const Node* findNode(const T&) const;

    static Node* copyNode(const Node*, const Node*);
    static void freeNode(const Node*) noexcept;
    static void updateParent(const Node*, Node*) noexcept;
    static void rotateLeft(Node*&) noexcept;
    static void rotateRight(Node*&) noexcept;
    static bool validHeap(const Node*) noexcept;
    bool validBST(const Node*, const T&, const T&) const noexcept;
public:
    class iterator {
        friend class Treap<T, Comp>;
        const Node* ptr;
        // Only the treap can construct iterators to itself
        iterator(const Node* ptr) noexcept : ptr{ ptr } {}
    public:
        // to-do: all typedefs & operators to qualify as bidirectional
        // iterator (or whatever category std::set<T>::iterator is)
        const T& operator*() const noexcept { return ptr->value; }
        const T* operator->() const noexcept { return &ptr->value; }
        iterator& operator++() noexcept;
        iterator& operator--() noexcept;
        iterator operator++(int) noexcept { auto copy = *this; ++(*this); return copy; }
        iterator operator--(int) noexcept { auto copy = *this; --(*this); return copy; }
        explicit operator bool() const noexcept { return (ptr->parent != ptr); } // not standard in STL
        auto operator<=>(const iterator&) const noexcept = default;
    };
    // to-do: why does this cause internal compiler errors :/
    Treap(/*const Comp& comp = Comp{}*/) noexcept(std::is_nothrow_copy_constructible_v<Comp>)
        : dummy{ _this_as_node() }, count{ 0 }, comp{ /*comp*/ } {}
    Treap(const Treap&);
    Treap& operator=(const Treap&);
    Treap(Treap&&) noexcept;
    Treap& operator=(Treap&&) noexcept;
    ~Treap() { clear(); }

    // Notes:
    // - sizeof(mt19937_64) == 5000 => not feasible to keep a copy inside each Treap
    // - having a static mt19937_64 per Treap type will lead to race conditions when using Treap from different threads :(
    // - maybe this is the way to go (how will we insert >1 value then?)
    void insert(Real pr, const T& value) { emplace(pr, value); }
    void insert(Real pr, T&& value) { emplace(pr, std::move(value)); }
    //template<class InputIt> // to-do: constrain w/ a concept
    //void insert(InputIt, InputIt);
    //void insert(std::initializer_list<T> il) { insert(il.begin(), il.end()); }
    
    // to-do: comment that the returned iterator is the next one
    iterator erase(iterator);
    iterator erase(iterator, iterator);
    iterator find(const T&) const;
    template<class... Args>
    void emplace(Real, Args&&...);

    iterator begin() const noexcept;
    iterator end() const noexcept;

    size_t size() const noexcept { return count; }
    bool empty() const noexcept { return (count == 0); }
    void clear() noexcept;

    void swap(Treap&) noexcept;
};

// iterator stuff
template<typename T, typename Comp>
auto Treap<T, Comp>::iterator::operator++() noexcept -> iterator& {
    if (ptr->right) { // find the leftmost right ancestor (if there are any)
        ptr = ptr->right;
        while (ptr->left) {
            ptr = ptr->left;
        }
    } else { // otherwise, find the smallest right predecessor by going as up-left as possible
        // parent pointers will never be nullptr!
        Node* parent = ptr->parent;
        while (/*ptr->parent &&*/ptr == parent->right) {
            ptr = parent;
            parent = parent->parent;
        }
        ptr = parent; // this is a step up-right
    }
    return *this;
}

template<typename T, typename Comp>
auto Treap<T, Comp>::iterator::operator--() noexcept -> iterator& {
    // symmetrical procedure for decrementing an iterator
    if (ptr->left) {
        ptr = ptr->left;
        while (ptr->right) {
            ptr = ptr->right;
        }
    } else {
        Node* parent = ptr->parent;
        while (ptr == parent->left) {
            ptr = parent;
            parent = parent->parent;
        }
        ptr = parent;
    }
    return *this;
}

template<typename T, typename Comp>
auto Treap<T, Comp>::begin() const noexcept -> iterator {
    const Node* location = _this_as_node();
    while (location->left) { // _this_as_node()->left is the same as this->root();
        location = location->left;
    }
    return { location };
}

template<typename T, typename Comp>
auto Treap<T, Comp>::end() const noexcept -> iterator {
    return { _this_as_node() };
}

// tree stuff
template<typename T, typename Comp>
void Treap<T, Comp>::copyFrom(const Treap& other) {
    vassert(!root());
    root() = copyNode(other.root(), _this_as_node());
    count = other.count;
    comp = other.comp;
}

template<typename T, typename Comp>
auto Treap<T, Comp>::findNode(const T& value) const -> const Node* {
    const Node* location = root();
    while (location) {
        if (comp(value, location->value)) {
            location = location->left;
        } else if (comp(location->value, value)) {
            location = location->right;
        } else {
            return location; //found it
        }
    }
    return nullptr; // not found
}

template<typename T, typename Comp>
auto Treap<T, Comp>::copyNode(const Node* ptr, const Node* parent) -> Node* {
    if (!ptr) {
        return nullptr;
    }
    Node* res = new Node(parent, ptr->pr, ptr->value);
    res->left = copyNode(ptr->left, res);
    res->right = copyNode(ptr->right, res);
    return res;
}

template<typename T, typename Comp>
void Treap<T, Comp>::freeNode(const Node* ptr) noexcept {
    if (ptr) {
        freeNode(ptr->left);
        freeNode(ptr->right);
        delete ptr;
    }
}

template<typename T, typename Comp>
void Treap<T, Comp>::updateParent(const Node* location, Node* newChild) noexcept {
    // "Removes" a node from the tree by replacing its
    // parent pointer to itself with a new child node.
    // Reminder that parent pointers are always non-null
    Node* parent = location->parent;
    if (location == parent->left) {
        parent->left = newChild;
    } else {
        parent->right = newChild;
    }
    if (newChild) {
        newChild->parent = parent;
    }
}

template<typename T, typename Comp>
void Treap<T, Comp>::rotateLeft(Node*& ptr) noexcept {
    // if the tree is empty or has no right subtree, rotating left is impossible
    if (!ptr || !ptr->right) {
        vassert(false); // (?)
        return;
    }
    Node* tmp = ptr->right;
    ptr->right = tmp->left;
    if (ptr->right) {
        ptr->right->parent = ptr;
    }
    tmp->parent = ptr->parent;
    tmp->left = ptr;
    ptr->parent = tmp;
    ptr = tmp;
}

template<typename T, typename Comp>
void Treap<T, Comp>::rotateRight(Node*& ptr) noexcept {
    // if the tree is empty or has no left subtree, rotating right is impossible
    if (!ptr || !ptr->left) {
        vassert(false); // (?)
        return;
    }
    Node* tmp = ptr->left;
    ptr->left = tmp->right;
    if (ptr->left) {
        ptr->left->parent = ptr;
    }
    tmp->parent = ptr->parent;
    tmp->right = ptr;
    ptr->parent = tmp;
    ptr = tmp;
}

template<typename T, typename Comp>
bool Treap<T, Comp>::validHeap(const Node* ptr) noexcept {
    return ((!ptr->left || (ptr->pr <= ptr->left->pr && validHeap(ptr->left)))
        && (!ptr->right || (ptr->pr <= ptr->right->pr && validHeap(ptr->right))));
}

template <typename T, typename Comp>
bool Treap<T, Comp>::validBST(const Node* ptr, const T& min, const T& max) const noexcept {
    return (!comp(ptr->value, min) && !comp(max, ptr->value)
        && (!ptr->left || validBST(ptr->left, min, ptr->value))
        && (!ptr->right || validBST(ptr->right, ptr->value, max)));
}

template<typename T, typename Comp>
Treap<T, Comp>::Treap(const Treap& other) : Treap() {
    copyFrom(other);
    vassert(dummy.parent == _this_as_node());
}

template<typename T, typename Comp>
auto Treap<T, Comp>::operator=(const Treap& other) -> Treap& {
    if (this != &other) {
        clear();
        copyFrom(other);
    }
    vassert(dummy.parent == _this_as_node());
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

/*template<typename T, typename Comp>
template<class InputIt>
void Treap<T, Comp>::insert(InputIt from, InputIt to) {
    while (from != to) {
        insert(*from);
        ++from;
    }
}*/

// to-do: this also doesn't depend on Comp type
// Ideally, it should delegate to some node function that does
// all the work and just call delete location afterwards
template<typename T, typename Comp>
auto Treap<T, Comp>::erase(iterator it) -> iterator {
    if (it == end()) { // bonus: in STL erase(end()) is undefined behaviour
        return it;
    }
    const Node* location = it.ptr;
    ++it;
    --count;
    // Bubble down the node to be deleted to a leaf & cut this leaf
    while (location->left && location->right) {
        Node* parent = location->parent;
        Node*& toLocation = (location == parent->left ? parent->left : parent->right);
        if (location->left->pr < location->right->pr) {
            // The left child should be above the right
            rotateRight(toLocation);
        } else {
            rotateLeft(toLocation);
        }
    }
    // If the node has one child, it's 1 rotation away from becoming
    // a leaf, so we directly link its parent to this child.
    if (location->left) {
        updateParent(location, location->left);
    } else if (location->right) {
        updateParent(location, location->right);
    } else {
        // No children => leaf node => cut it & let its parent know
        updateParent(location, nullptr);
    }
    // Finally, free the memory in question & return the updated iterator
    delete location;
    if (root()) {
        vassert(validHeap(root()));
        vassert(validBST(root(), *begin(), *--end()));
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
    const Node* location = findNode(value);
    return (location ? iterator(location) : end());
}

template<typename T, typename Comp>
template<class ...Args>
void Treap<T, Comp>::emplace(Real pr, Args&&... _args) {
    Node* newNode = new Node(nullptr, pr, std::forward<Args>(_args)...);
    const T& newVal = newNode->value;
    Node* location = root();
    Node* parent = _this_as_node(); // <=> root()->parent, but faster
    bool isLeft = true; // Whether location is the left or right child of its root
    while (location) {
        parent = location;
        if (comp(newVal, location->value)) {
            location = location->left;
            isLeft = true;
        } else {
            location = location->right;
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
    // Bubble up the newly inserted note via rotations until
    // the heap property is restored (or it reaches the root)
    while (parent != _this_as_node() && location->pr < parent->pr) {
        // Parent node is a valid, "full" one => rotate the new node up to its position
        Node* grandParent = parent->parent;
        Node*& toParent = (grandParent->left == parent ? grandParent->left : grandParent->right);
        if (parent->left == location) { // left child -> rotate right
            rotateRight(toParent);
            vassert(parent == location->right);
        } else { // right child -> rotate left
            rotateLeft(toParent);
            vassert(parent == location->left);
        }
        vassert(toParent == location && grandParent == location->parent);
        parent = grandParent;
    }
    ++count;
    vassert(validHeap(root()));
    vassert(validBST(root(), *begin(), *--end()));
    // to-do: return iterator to the inserted value
}

template<typename T, typename Comp>
void Treap<T, Comp>::clear() noexcept {
    freeNode(root());
    root() = nullptr;
    count = 0;
}

template<typename T, typename Comp>
void Treap<T, Comp>::swap(Treap& other) noexcept {
    using std::swap;
    swap(root(), other.root());
    swap(count, other.count);
    swap(comp, other.comp);
    vassert(dummy.parent == _this_as_node());
    vassert(other.dummy.parent == other._this_as_node());
}

template<typename T, typename Comp>
void swap(Treap<T, Comp>& lhs, Treap<T, Comp>& rhs) noexcept {
    lhs.swap(rhs);
}
