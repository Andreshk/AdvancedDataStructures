#pragma once
#include <array>
#include <atomic>
#include <cstdint> // size_t
#include <type_traits> // std::is_trivial_v
#include <iterator>
#include <bit> // std::{has_single_bit, countr_zero}

/* TO-DO:
 * - maybe tails can be reused on push_back, rather than pop_back
 *   => should tails be immediately inserted into the tree then?
 * - transient operations should do path copying when part of the path to the modified value
 *   is shared (i.e. ref-counter > 1) (!) => update the comments on transients afterwards
 * - add ctor from a range (maybe continuous?) that builds a tree & memcpy's elements inside
 * x add support for non-trivial types via shared pointer (own or standard) - too much work...
 *   => then tails couldn't be reused neither on pop_back(), nor push_back()
 */

 // Enable this for logging of node allocations & deallocations.
#define ENABLE_NODECOUNTER
#ifdef ENABLE_NODECOUNTER
#include "PersistentVectorNodeCounter.h" // You can find this at https://github.com/Andreshk/SnippySnippets.
#else
#define DEBUG_ONLY(expr) ((void)0)
#endif // ENABLE_NODECOUNTER

// Calculate the number of values that fit in a leaf node (dependent on branching factor)
template <typename T, size_t B>
consteval size_t getL() {
    return (B * sizeof(void*)) / sizeof(T);
}

/* PersistentVector is a tree with a large branching factor (usually 32), such that only
 * the leaf nodes contain values & every leaf node is the same distance from the root.
 * Operations on values like modifying, inserting & removing are done via path copying,
 * and take O(log_32_n) - practically constant time. Also supported are _transient_
 * operations on rvalues (either temporaries, or explicitly received via make_transient()).
 * These operations modify the values in-place, instead of copying the path - this means
 * they may affect other vectors that share the same structure (!). Transient operations
 * are best used in tight loops on vectors that do not share their structure with others.
 * Example usage:
 *   PersistentVector<int> pv1;
 *   auto pv2 = pv1.push_back(5);
 *   const auto pv3 = pv2.push_back(123);
 *   make_transient(pv2).set(0,42); // pv2[0] = 42 -> no path copying, may cause pv3[0] == 42
 *   PersistentVector<int> pv4;
 *   for (int i = 0; i < 100; ++i) {
 *     make_transient(pv4).push_back(i); // no unneeded allocations (!)
 *   }
 */
template <typename T, size_t B = 32>
    requires (std::is_trivial_v<T>   // Only trivial types can be contained - consider using pointers to T instead.
           && std::has_single_bit(B) // Branch factor can only be a power of 2
           && B >= 2 && B <= 256     // ...but not too small or too large
           && getL<T, B>() >= 2)     // Values too large to fit in leaves - increase branch factor or store pointers.
class PersistentVector {
    static const size_t logB = std::countr_zero(B); // Logarithm of the branch factor
    static const size_t numValuesInLeaf = getL<T, B>();

    struct Node {
        std::atomic<size_t> refCount;
        union {
            // The calculations above guarantee that these arrays are as close as possible
            // in size, i.e. there is (ideally) no space left unused in the nodes.
            // This works best when sizeof(T) is a power of 2.
            std::array<Node*, B> ptrs;
            std::array<T, numValuesInLeaf> values; 
        };

        Node() {
            DEBUG_ONLY(NodeCounter::add(this));
            ptrs.fill(nullptr);
            refCount = 1;
        }
        Node(const Node* other, const size_t height) {
            DEBUG_ONLY(NodeCounter::add(this));
            refCount = 1;
            if (height > 0) {
                ptrs = other->ptrs;
                for (size_t i = 0; i < ptrs.size() && ptrs[i]; ++i) {
                    ++ptrs[i]->refCount;
                }
            } else {
                values = other->values;
            }
        }
        Node(const Node&) = delete;

        // Instead of having a shared_ptr class, all pointers manually call Node::acquire()
        // and Node::release() before being assigned a new value or being destroyed.
        Node* acquire() {
            if (++refCount == 1) {
                assert(false && "Race condition: acquiring a node during its destruction!");
            }
            return this;
        }
        // The height of the Node determines whether this is a leaf node, or recursive calls are needed.
        void release(const size_t height) {
            if (--refCount > 0)
                return;
            if (height > 0) {
                for (size_t i = 0; i < ptrs.size() && ptrs[i]; ++i) {
                    ptrs[i]->release(height - 1);
                }
            }
            DEBUG_ONLY(NodeCounter::remove(this));
            delete this;
        }
        // Find the last non-null pointer to a subtree
        size_t findLastSubtree() const {
            assert(ptrs[0] != nullptr);
            size_t idx = ptrs.size() - 1;
            while (idx > 0 && ptrs[idx] == nullptr) {
                --idx;
            }
            return idx;
        }
    };

    // For every PersistentVector, the last leaf node (a.k.a. the tail) is held outside of the
    // tree structure, accessible via the root pointer. After some operations the tail node can
    // be shared with other vectors, and be a part of some other vector's tree structure.
    Node* root;
    Node* tail;
    size_t height; // root node height
    size_t n; // number of values contained

    // Given a value index, find the index of the pointer at a given level in the path to this value
    static size_t idxAtLevel(const size_t idx, const size_t height) {
        assert(height > 0 && "Use idx%numValuesInLeaf for value access at leaf nodes");
        static const size_t branchMask = B - 1; // Used during indexing operations
        return (((idx / numValuesInLeaf) >> ((height - 1)*logB)) & branchMask);
    }
    // The number of elements, contained in the tree structure (i.e. excluding the tail)
    static size_t treeSize(const size_t n) {
        // Round down to the nearest multiple of numValuesInLeaf
        static const size_t leafMask = numValuesInLeaf - 1;
        return (n&~leafMask); // <=> (n - n%numValuesInLeaf)
    }
    // The number of elements a full tree of a given height will contain
    static size_t fullTreeSize(const size_t height) {
        return (numValuesInLeaf << (height*logB));
    }
    // Check whether the element at a given index resides in the tail node
    bool insideTail(const size_t i) const {
        return (i >= treeSize(n));
    }
    // Find the leaf node (this includes the tail), containing a given index
    Node* findLeaf(const size_t i, const size_t min_height = 0) const {
        assert(i <= n); // i == n is allowed only for end iterator construction
        if (insideTail(i)) {
            return tail;
        } else { // Walk down the tree, rooted in currRoot, knowing its height is currH
            Node* currRoot = root;
            size_t currH = height;
            while (currH > min_height) {
                currRoot = currRoot->ptrs[idxAtLevel(i, currH)];
                --currH;
            }
            return currRoot;
        }
    }
    // Return a modifying reference to a value at a given index, so it could be used in transient and non-transient operations
    T& find(const size_t i) const {
        assert(i <= n); // i == n is allowed only for end iterator construction
        return findLeaf(i)->values[i%numValuesInLeaf];
    }
    // Copy the path to a leaf, containing a given index in the tree, rooted in root. The last
    // parameter determines at which level the copying should be stopped (0 for leaf, 1 for a
    // leaf's parent). Also saves the address to the last copied node, for quicker access later.
    static Node* copyPath(const Node* root, const size_t height, Node*& lastNode, const size_t i, const size_t min_height = 0) {
        assert(root != nullptr);
        Node* newRoot = new Node(root, height);
        if (height == min_height) {
            // The newly allocated node actually contains values, copied from root
            lastNode = newRoot;
        } else {
            const size_t idx = idxAtLevel(i, height);
            --newRoot->ptrs[idx]->refCount; // This has been increased unnecessarily
            newRoot->ptrs[idx] = copyPath(root->ptrs[idx], height - 1, lastNode, i, min_height);
        }
        return newRoot;
    }
    // Build a new leftmost branch with a given height, having newLeaf as the only leaf.
    static Node* buildLeftBranch(const size_t height, Node* newLeaf) {
        if (height == 0) {
            return newLeaf;
        } else {
            Node* ptr = new Node;
            ptr->ptrs[0] = buildLeftBranch(height - 1, newLeaf);
            return ptr;
        }
    }
    // Check whether the last subtree of a given height is full
    static bool hasFullSubtrees(const size_t n, const size_t height) {
        return (n % fullTreeSize(height) == 0);
    }
    // Insert (adopt) a leaf into a tree with a given root, height & value count. It is known
    // that the tree has space for one more leaf. newLeaf will be adopted as the inserted leaf.
    static Node* insertLeaf(const Node* root, Node* newLeaf, const size_t height, const size_t n) {
        assert(!hasFullSubtrees(n, height));
        assert((root == nullptr) == (height == 0));
        if (height == 0)
            return newLeaf;
        Node* newRoot = new Node(root, height);
        const size_t idx = root->findLastSubtree();
        if (hasFullSubtrees(n, height - 1)) {
            // The last subtree of root is full => make a new branch next to it & return
            newRoot->ptrs[idx + 1] = buildLeftBranch(height - 1, newLeaf);
        } else {
            // There is space for a leaf in the last subtree => recurse
            --newRoot->ptrs[idx]->refCount;
            newRoot->ptrs[idx] = insertLeaf(root->ptrs[idx], newLeaf, height - 1, n);
        }
        return newRoot;
    }
    // Check internal structure invariants. No-op when assert is not defined
    void checkInvariants() const {
        if (n < numValuesInLeaf) {
            assert(root == nullptr && height == 0 && "Tree should be empty when all values can fit into the tail!");
        } else if (height == 0) {
            assert(n < 2 * numValuesInLeaf && "The tail should not be full!");
        } else {
            assert(root->ptrs[1] != nullptr && "The root node should always have more than one child!");
            const size_t numLeaves = n / numValuesInLeaf;
            assert(numLeaves <= (fullTreeSize(height) / numValuesInLeaf) && "Tree too small - is the tail full?");
            assert(numLeaves > (fullTreeSize(height - 1) / numValuesInLeaf) && "Tree unnecessarily high!");
        }
    }
    // Helper struct to guarantee invariant checking when exiting the transient methods.
    struct CheckInvariantsRAII {
        const PersistentVector& pv;
        [[nodiscard]] CheckInvariantsRAII(const PersistentVector& pv) : pv{ pv } {}
        ~CheckInvariantsRAII() { pv.checkInvariants(); }
    };
    friend CheckInvariantsRAII;

    PersistentVector(Node* root, Node* tail, size_t height, size_t n)
        : root{ root }, tail{ tail }, height{ height }, n{ n }
    { // This constructor should only take care of setting the members, without any ref-counting!
        checkInvariants();
    }
public:
    // The manual ref-counting requires non-default special member functions (big 6)
    PersistentVector() : PersistentVector(nullptr, new Node, 0, 0) {}
    // Copying is O(1), just increase the refcounts
    PersistentVector(const PersistentVector& other)
        : PersistentVector{ (other.root ? other.root->acquire() : nullptr), other.tail->acquire(), other.height, other.n } {}
    PersistentVector& operator=(const PersistentVector& other) {
        if (this != &other) {
            this->~PersistentVector();
            new (this) PersistentVector(other);
        }
        return *this;
    }
    // Moving is trivial
    PersistentVector(PersistentVector&& other) noexcept : PersistentVector{} { swap(other); }
    PersistentVector& operator=(PersistentVector&& other) noexcept {
        if (this != &other) {
            swap(other);
        }
        return *this;
    }
    ~PersistentVector() {
        if (root) {
            root->release(height);
            root = nullptr;
        }
        tail->release(0); // Usually tail->refCount == 1, but it can be shared on pop_back()
        tail = nullptr;
        height = n = 0;
    }

    // Random access in O(log_B_n) => in practice, no more than 5-6 node hops.
    // Each node hop may cost 2 cache misses (not 1), since a node may not fit in a cache line,
    // making access to the last pointer in the array probably costlier than access to the first.
    const T& operator[](const size_t i) const {
        assert(i < n);
        return find(i);
    }
    // Return a new vector, with value val at position i
    PersistentVector set(const size_t i, const T& val) const& {
        assert(i < n);
        if (insideTail(i)) {
            Node* newRoot = (root ? root->acquire() : nullptr);
            Node* newTail = new Node(tail, 0);
            newTail->values[i%numValuesInLeaf] = val;
            return { newRoot,newTail,height,n };
        } else {
            assert(root != nullptr);
            Node* newLeaf = nullptr;
            Node* newRoot = copyPath(root, height, newLeaf, i);
            newLeaf->values[i%numValuesInLeaf] = val;
            // The tree will be modified, but the tail will stay the same & can be shared
            return { newRoot,tail->acquire(),height,n };
        }
    }
    // Transient set() - basically a modifying operation, called only on temporaries
    // as an optimization, or when manually specified via make_transient(...).set().
    // Note that operations on transients do not return either a copy, nor a reference.
    // Important: such changes to one object will affect all object that share their
    // structure with the modified one (!). This is true for all transient operations!
    void set(const size_t i, const T& val) && {
        assert(i < n);
        find(i) = val;
    }

    // Return a new vector, obtained by pushing back a value
    PersistentVector push_back(const T& val) const& {
        // When the tail fills up, it's immediately inserted in the tree as a leaf, and we create an
        // empty tail for the new vector => there will always be space in it for the next push_back
        // operation. The same invariant (non-full tail) is maintained after pop_back(), too.
        Node* newTail = new Node(tail, 0);
        newTail->values[n%numValuesInLeaf] = val;
        
        if (n%numValuesInLeaf != numValuesInLeaf - 1) {
            // There is more space left in the tail => nothing more to do (no more allocations,
            // path copying or anything). Note that this happens almost every time (!)
            Node* newRoot = (root ? root->acquire() : nullptr);
            return { newRoot,newTail,height,n + 1 };
        } else if (root == nullptr) {
            // The tree structure is empty
            assert(height == 0 && n == numValuesInLeaf - 1);
            return { newTail,new Node,0,n + 1 };
        } else if (treeSize(n) == fullTreeSize(height)) {
            // The tree structure is full -> the new tree (with the added leaf) will be higher
            Node* newRoot = new Node;
            newRoot->ptrs[0] = root->acquire();
            newRoot->ptrs[1] = buildLeftBranch(height, newTail);
            assert(newRoot->ptrs[1]->refCount == 1);
            return { newRoot,new Node,height + 1,n + 1 };
        } else {
            // There is space in the tree for another leaf
            assert(height > 0);
            Node* newRoot = insertLeaf(root, newTail, height, treeSize(n));
            assert(newRoot->refCount == 1 && newTail->refCount == 1);
            return { newRoot,new Node,height,n + 1 };
        }
    }
    // Transient (modifying) push_back()
    void push_back(const T& val) && {
        CheckInvariantsRAII check{ *this };
        // See comments for non-transient push_back() - the same invariants are maintained here
        tail->values[n%numValuesInLeaf] = val;
        ++n;
        if (n%numValuesInLeaf != 0) { // Tail isn't full yet, literally nothing more to do
            return;
        } else if (root == nullptr) {
            assert(height == 0 && n == numValuesInLeaf);
            root = tail;
            tail = new Node;
            height = 0;
        } else if (treeSize(n - 1) == fullTreeSize(height)) {
            Node* newRoot = new Node;
            newRoot->ptrs[0] = root;
            newRoot->ptrs[1] = buildLeftBranch(height, tail);
            root = newRoot;
            tail = new Node;
            ++height;
        } else {
            assert(height > 0);
            Node* currRoot = root;
            size_t currH = height;
            --n; // The old value is needed for intermediate calculations
            bool inserted = false;
            // This essentially finds the parent of findLeaf(n),
            // stopping early if a new branch (or part of it) is needed
            while (currH > 1) {
                const size_t idx = idxAtLevel(n, currH);
                if (currRoot->ptrs[idx]) {
                    currRoot = currRoot->ptrs[idx];
                    --currH;
                } else {
                    currRoot->ptrs[idx] = buildLeftBranch(currH - 1, tail);
                    inserted = true;
                    break;
                }
            }
            if (!inserted) {
                Node*& ptrToLeaf = currRoot->ptrs[idxAtLevel(n, 1)];
                assert(ptrToLeaf == nullptr);
                ptrToLeaf = tail;
            }
            tail = new Node;
            ++n;
        }
    }
    // Return a new vector, obtained by removing the last value
    PersistentVector pop_back() const& {
        assert(n > 0);
        if (n%numValuesInLeaf != 0) { // The tail isn't empty
            // Normally, a vector keeps its own tail -> but for trivial types it is okay to reuse a part of
            // another tree's structure, since pushing back again will allocate a new tail node anyway.
            // Note that this, too, happens almost every time (!)
            Node* newRoot = (root ? root->acquire() : nullptr);
            return { newRoot,tail->acquire(),height,n - 1 };
        } else if (height == 0) {
            // After removing an element, the remaining fit in a single node (the tail) => we'll have an empty tree
            assert(n == numValuesInLeaf);
            return { nullptr,root->acquire(),0,n - 1 };
        } else if (n == (fullTreeSize(height - 1) + numValuesInLeaf)) {
            // After removing an element, the remaining can fit in a tree, 1 level shorter
            Node* newRoot = root->ptrs[0]->acquire();
            Node* newTail = findLeaf(n - 1)->acquire();
            return { newRoot,newTail,height - 1,n - 1 };
        } else {
            // Removing an element means extracting the last leaf of the tree as the new tail.
            // This means copying the path up until the parent of said leaf, caching it to be reused
            // as the new tail, and replacing the pointer to it in the copied path with nullptr.
            Node* leafParent = nullptr;
            Node* newRoot = copyPath(root, height, leafParent, n - 1, 1);
            Node*& ptrToLeaf = leafParent->ptrs[idxAtLevel(n - 1, 1)];
            Node* newTail = ptrToLeaf;
            ptrToLeaf = nullptr;
            return { newRoot,newTail,height,n - 1 };
        }
    }
    // Transient (modifying) pop_back()
    void pop_back() && {
        assert(n > 0);
        CheckInvariantsRAII check{ *this };
        if (n%numValuesInLeaf != 0) {
            // The tail was not empty => reusing the tail node
            // leaves us with literally nothing else to do
            --n;
        } else if (height == 0) {
            // After removing an element, the remaining fit in a single node (the tail) => we'll have an empty tree
            assert(n == numValuesInLeaf);
            tail->release(0);
            tail = root;
            root = nullptr;
            --n;
        } else if (n == (fullTreeSize(height - 1) + numValuesInLeaf)) {
            // After removing an element, the remaining can fit in a tree, 1 level shorter
            Node* newRoot = root->ptrs[0]->acquire();
            Node* newTail = findLeaf(n - 1)->acquire();
            root->release(height);
            tail->release(0);
            root = newRoot;
            tail = newTail;
            --height;
            --n;
        } else {
            // Extract the last leaf from the tree by resetting the pointer to it in its parent's node.
            Node* leafParent = findLeaf(n - 1, 1);
            Node*& ptrToLeaf = leafParent->ptrs[idxAtLevel(n - 1, 1)];
            tail->release(0);
            tail = ptrToLeaf;
            ptrToLeaf = nullptr;
            --n;
        }
    }
    
    // Some standard methods...
    size_t size() const { return n; }
    bool empty() const { return (n == 0); }
    void clear() { *this = PersistentVector{}; }
    void swap(PersistentVector& other) {
        std::swap(root, other.root);
        std::swap(tail, other.tail);
        std::swap(height, other.height);
        std::swap(n, other.n);
    }

    // Facilitates quick iteration
    class iterator {
        friend PersistentVector;
    private:
        // Invariant: ptr == &pv[idx] as long as *this is not the end() iterator.
        // This way dereferencing an iterator does not involve a tree walk and is
        // always equivalent to dereferencing a single pointer, and incrementing
        // an iterator is almost always as fast as incrementing two integers - the
        // pointer is updated via a tree walk only every numValuesInLeaf increments.
        const PersistentVector* pv;
        const T* ptr;
        size_t idx;

        iterator(const PersistentVector* pv, const size_t idx)
            : pv{ pv }, ptr{ (idx == -1 ? nullptr : & pv->find(idx)) }, idx{idx} {}
    public:
        using value_type = T; // value_type should be non-const even for const iterators
        using pointer    = const T*;
        using reference  = const T&;
        using difference_type   = ptrdiff_t;
        using iterator_category = std::random_access_iterator_tag;
        using iterator_concept  = std::random_access_iterator_tag;
        iterator() : pv{ nullptr }, ptr{ nullptr }, idx{ -1 } {}

        reference operator*() const { return *ptr; }
        pointer operator->() const { return ptr; }
        iterator& operator++() {
            ++ptr; ++idx;
            if (idx%numValuesInLeaf == 0) { // We have moved to the next leaf in the tree
                ptr = &pv->find(idx); // This will take care of bounds checking
            }
            return (*this);
        }
        iterator operator++(int) {
            iterator copy{ *this };
            ++(*this);
            return copy;
        }
        iterator& operator--() {
            --ptr; --idx;
            if (idx%numValuesInLeaf == numValuesInLeaf - 1) { // We have moved to the previous leaf in the tree
                ptr = &pv->find(idx); // This will take care of bounds checking
            }
            return (*this);
        }
        iterator operator--(int) {
            iterator copy{ *this };
            --(*this);
            return copy;
        }
        iterator& operator+=(const ptrdiff_t n) {
            const size_t oldIdx = idx;
            ptr += n; idx += n;
            if (idx > pv->size()) {
                ptr = nullptr;
            } else if (idx / numValuesInLeaf != oldIdx / numValuesInLeaf) {
                ptr = &pv->find(idx);
            }
            return *this;
        }
        iterator operator+(const ptrdiff_t n) const {
            iterator copy{ *this };
            copy += n;
            return copy;
        }
        friend iterator operator+(const ptrdiff_t n, const iterator& it) { return (it + n); }
        iterator& operator-=(const ptrdiff_t n) { return (*this) += (-n); } // lol
        iterator operator-(const ptrdiff_t n) const { return (*this) + (-n); }
        ptrdiff_t operator-(const iterator& other) const { return (idx - other.idx); }
        reference operator[](const ptrdiff_t n) const { return (*pv)[idx + n]; }
        // Although we can detect when iterators from different vectors are compared,
        // the requirement for iterators to be totally ordered means we can't use it to always return false.
        bool operator==(const iterator& other) const { assert(pv == other.pv); return (idx == other.idx); }
        auto operator<=>(const iterator& other) const { assert(pv == other.pv); return (ptr <=> other.ptr); }
    };
    using const_iterator = iterator;
    iterator begin() const { return { this,0 }; }
    iterator   end() const { return { this,n }; }
    const_iterator cbegin() const { return begin(); }
    const_iterator   cend() const { return   end(); }
    static_assert(std::random_access_iterator<iterator>);
};

template <typename T, size_t B>
PersistentVector<T, B>&& make_transient(PersistentVector<T, B>& pv) {
    return std::move(pv);
}
