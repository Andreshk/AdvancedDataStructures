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
    T extractMin() noexcept {
        using handle = DList<FNode<T>>::handle;
        // 44 is the max degree such that F(d+2) < INT_MAX.
        // For UINT_MAX it's 45, and 91 for UINT64_MAX 
        std::array<handle, 45> trees = {};
        auto root = roots.front();
        // Orphan the subtrees before making them roots
        for (auto& t : root->subtrees) { t.parent = {}; }
        roots.append(std::move(root->subtrees));
        T res = std::move(root->val);
        roots.remove(root);
        // This is called "consolidation" in the papers & textbooks
        while (!roots.empty()) {
            handle curr = roots.extract(roots.front());
            int deg = curr->subtrees.size();
            // If there's a tree with the same degree, merge with the current.
            // This is the same as merging trees into a binomial heap :)
            // Each merge reduces the potential by one, making up for the actual time spent.
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
    bool decreaseKey(iterator it, const T& newVal) {
        if (!comp(newVal, *it)) { // This is not a decrease
            return false;
        }
        auto curr = it.it;
        curr->val = newVal;
        bool toRemove = curr->parent && comp(newVal, curr->parent->val);
        bool newMin = comp(newVal, roots.front()->val);
        // The first iteration may cut a node and the last (may also be the first) may mark one,
        // but each iteration between them will unmark & cut a node, decreasing the potential by 1.
        // This decrease makes up for the actual time spent :)
        while (curr->parent && toRemove) {
            // Cut the current node & add it to the roots list
            curr->marked = false; // See CUT(H,x,y) in CLRS
            auto parent = std::exchange(curr->parent, {});
            roots.insert(parent->subtrees.extract(curr));
            // The first cut node (with the new value) is the newly decreased key and may become the new minimum,
            // but the remaining are non-roots and not changed, so will not become minimums when cut.
            if (newMin) {
                roots.rotate(curr);
                newMin = false;
            }
            // Unless the parent is a root, mark it and record whether it was marked before
            toRemove = parent->parent && std::exchange(parent->marked, true);
            // Move up
            curr = parent;
        }
        return true;
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
    // Checks the heap property of each tree & whether every node has the correct parent pointer.
    // Obviously O(n), use for testing only.
    bool validate() const {
        return [&](this auto&& self, const DList<FNode<T>>& nodes, const DList<FNode<T>>::loop_iterator parent) -> bool {
            // This is std::ranges::all
            for (auto it = nodes.begin(); it != nodes.end(); ++it) {
                if ((parent && comp(it->val, parent->val)) || it->parent != parent || !self(it->subtrees, it)) {
                    return false;
                }
            }
            return true;
        }(roots, {});
    }

    // Pretty-printing
    friend struct fmt::formatter<FibonacciHeap<T>>;
};
