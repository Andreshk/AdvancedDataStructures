#pragma once
#include <cassert>
#include <utility> // std::exchange
#include <array>
#include "DList.h"

// All Fibonacci heap nodes reside in some doubly-linked circular list,
// so they don't need explicit prev/next pointers.
template <typename T>
struct FNode {
    T val;
    bool marked = false;
    DList<FNode>::iterator parent;
    DList<FNode> subtrees;
    FNode(const T& val) : val{ val } {}
};

template <typename T, typename Comp = std::less<T>>
class FibonacciHeap {
    DList<FNode<T>> roots;
    int numValues = 0;
    [[msvc::no_unique_address]] Comp comp = {};
public:
    FibonacciHeap() = default;
    FibonacciHeap(FibonacciHeap&& other) noexcept
        : roots{ std::move(other.roots) }, numValues{ std::exchange(other.numValues, 0) }, comp{ std::move(other.comp) } {}
    FibonacciHeap& operator=(FibonacciHeap&& other) noexcept {
        roots = std::move(other.roots);
        numValues = std::exchange(other.numValues, 0);
        comp = std::move(other.comp);
        return *this;
    }
    ~FibonacciHeap() { numValues = 0; }

    // A heap "iterator" is just a const-wrapper around a DList iterator.
    // There is no iteration possible - use only for dereferencing & decreaseKey()
    class iterator {
        DList<FNode<T>>::iterator it;
        friend class FibonacciHeap; // Only the heap can construct iterators to itself
        iterator(DList<FNode<T>>::iterator it) : it{ it } {}
    public:
        iterator() = default;
        const T& operator*() const { return it->val; }
        const T* operator->() const { return &it->val; }
        operator bool() const { return bool(it); }
        bool operator==(const iterator&) const = default;
    };

    // Inserts a value into the heap. O(1) amortized time
    iterator insert(const T& val) {
        auto it = roots.insert(FNode{ val });
        const bool newMin = !roots.empty() && comp(val, roots.front()->val);
        if (newMin) { // The minimum should be reachable in O(1)
            roots.rotate(it);
        }
        ++numValues;
        return { it };
    }
    // Removes & returns the minimum value in the heap. O(lgn) amortized time, O(n) worst-case (!)
    // Can be noexcept only if it didn't copy the valule inside.
    T extractMin() {
        using handle = DList<FNode<T>>::handle;
        std::array<handle, 32> trees = {};
        // The minimum's subtrees & the other trees are processed the same way
        auto addTrees = [&](DList<FNode<T>>& ts) {
            while (!ts.empty()) {
                handle curr = ts.extract(ts.front());
                int deg = curr->subtrees.size();
                // If there's a tree with the same degree, merge with the current.
                // This is the same as merging trees into a binomial heap :)
                while (trees[deg]) {
                    if (comp(curr->val, trees[deg]->val)) {
                        trees[deg]->parent = curr.toIter();
                        trees[deg]->marked = false;
                        curr->subtrees.insert(std::move(trees[deg]));
                    } else {
                        curr->parent = trees[deg].toIter();
                        curr->marked = false;
                        trees[deg]->subtrees.insert(std::move(curr));
                        curr = std::move(trees[deg]);
                    }
                    ++deg;
                    assert(curr->subtrees.size() == deg);
                }
                trees[deg] = std::move(curr);
            }
        };
        auto root = roots.front();
        addTrees(root->subtrees);
        T res = root->val;
        roots.remove(root);
        addTrees(roots);
        // Add all different-degree trees to the roots list,
        // simultaneously finding the one with the minimum value.
        assert(!root); // Will be reused for the new minimum
        for (handle& hnd : trees) {
            if (hnd) {
                if (!root || comp(hnd->val, root->val)) {
                    root = hnd.toIter();
                }
                roots.insert(std::move(hnd));
            }
        }
        // Rotate the new roots list to the minimum value
        roots.rotate(root);
        --numValues;
        return res;
    }
    // Decreases the value in the heap, pointed by the given iterator. O(1) amortized time
    void decreaseKey(iterator it, const T& newVal) {
        assert(!comp(*it, newVal)); // This is not an "increase"
        auto curr = it.it, parent = curr->parent;
        curr->val = newVal;
        bool toRemove = parent && comp(newVal, parent->val);
        bool first = true;
        // Each iteration except the first and last unmarks & cuts a node, decreasing the potential by 1.
        // This decrease makes up for the actual time spent :)
        while (parent && toRemove) {
            assert(parent == curr->parent); // Convenience
            // Cut the current node & add it to the roots list
            auto hnd = parent->subtrees.extract(curr);
            hnd->marked = false; // See CUT(H,x,y) in CLRS
            roots.insert(std::move(hnd));
            // The first new root is the newly decreased key and may become the new minimum
            if (first && comp(curr->val, roots.front()->val)) {
                roots.rotate(curr);
            }
            // Mark the parent, but record whether it was marked before
            toRemove = std::exchange(parent->marked, true);
            // Move up and cut the new root's link to its previous parent
            curr = std::exchange(parent, {});
            parent = curr->parent;
            // The remaining nodes to be cut are non-root nodes,
            // and are surely >= than the corresponding root, so can't become the new min root
            first = false;
        }
    }
    // Returns the minimum value in the heap
    const T& peekMin() const noexcept {
        assert(!empty());
        return roots.front()->val;
    }
    // Checks whether the heap is empty
    bool empty() const noexcept {
        assert(roots.empty() == (numValues == 0));
        return roots.empty();
    }
    // Returns the # of values in the heap
    int size() const noexcept { return numValues; }
    // Merge another heap into the current. O(1) time (total potential does not increase)
    void merge(FibonacciHeap&& other) {
        assert(this != &other);
        if (other.empty()) {
            return;
        } else if (empty()) {
            *this = std::move(other);
            return;
        } else if (comp(peekMin(), other.peekMin())) {
            roots.append(std::move(other.roots));
        } else {
            other.roots.append(std::move(roots));
            roots = std::move(other.roots);
        }
        numValues += std::exchange(other.numValues, 0);
    }

    // Pretty-printing
    friend struct fmt::formatter<FibonacciHeap<T>>;
};
