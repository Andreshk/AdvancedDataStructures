#pragma once
#include <iostream> // for visualization
#include <bit> // std::countr_zero
#include <utility> // std::integer_sequence

// Implementation of binomial heaps, parameterized by their size.
// Since template arguments should be resolved at compilation time,
// only heaps of a static size, known at compilation, can be constructed.
// Everything can therefore be constexpr, but specifiers are omitted for brevity.
// On a positive note, when the types contain all the information, the structure
// is as space efficient as possible - sizeof(BinomialHeap<N>) == N*sizeof(int)
// (on compilers where [[no_unique_address]] is supported).
namespace Meta {
// Array of binomial trees with decreasing ranks in [Rank..0]
template <unsigned Rank>
struct BinomialTreeArray;
// Binomial tree of a given rank
template <unsigned Rank>
struct BinomialTree;

// Recursion base case - a tree of rank 0 has a single value
template <>
struct BinomialTree<0> {
    int value;

    static constexpr unsigned Size = 1;
    bool valid() const { return true; }
};
// Otherwise, a tree of rank R contains an array of trees with ranks [R-1..0]
template <unsigned Rank>
struct BinomialTree {
    int value;
    BinomialTreeArray<Rank - 1> subtrees;

    static constexpr unsigned Size = 1 + decltype(subtrees)::Size;
    static_assert(Size == (1u << Rank)); // sanity check
    bool valid() const { return subtrees.valid(value); }
};

// The array of binomial trees with decreasing ranks is, of course, defined recursively.
template <>
struct BinomialTreeArray<0> {
    BinomialTree<0> t;

    static constexpr unsigned Size = decltype(t)::Size;
    bool valid(const int root) const { return !(t.value < root); }
};

template <unsigned Rank>
struct BinomialTreeArray {
    BinomialTree<Rank> t;
    BinomialTreeArray<Rank - 1> ts;

    static constexpr unsigned Size = decltype(t)::Size + decltype(ts)::Size;
    bool valid(const int root) const { return (!(t.value < root) && t.valid() && ts.valid(root)); }
};

// Only trees of the same rank can be merged (into a tree of a rank larger)
template <unsigned Rank>
BinomialTree<Rank + 1> mergeTrees(const BinomialTree<Rank>& lhs, const BinomialTree<Rank>& rhs) {
    if constexpr (Rank == 0) { // <=> BinomialTree base case 
        if (lhs.value < rhs.value) {
            return { lhs.value, {rhs} };
        } else {
            return { rhs.value, {lhs} };
        }
    } else {
        if (lhs.value < rhs.value) {
            return { lhs.value, {rhs, lhs.subtrees} };
        } else {
            return { rhs.value, {lhs, rhs.subtrees} };
        }
    }
}

// A binomial heap of size N is also a (recursively defined) array of
// binomial trees with increasing ranks, corresponding to each bit of N.
template <unsigned N>
struct BinomialHeap;

// Empty binomial heap - useful only as a base case for the reursive definition.
template <>
struct BinomialHeap<0> {
    static constexpr unsigned Size = 0;
    static bool valid() { return true; }
    static int getMin() { return INT_MAX; }
};

template <unsigned N>
struct BinomialHeap {
    BinomialTree<std::countr_zero(N)> t; // countr_zero returns the index of N's lowest bit
    [[no_unique_address]] // Empty heaps are empty structs & should not increase sizeof(Heap) (!)
    BinomialHeap<N & (N - 1)> ts; // this removes N's lowest bit

    static constexpr unsigned Size = N;
    static_assert(Size == decltype(t)::Size + decltype(ts)::Size); // sanity check
    int getMin() const { return std::min(t.value, ts.getMin()); }
    bool valid() const { return (t.valid() && ts.valid()); }
};

// Special case of the general merge algorithm - of a binomial heap & a single tree.
// Standard algorithm of inserting a value in an ordered list.
template <unsigned N, unsigned Rank>
BinomialHeap<N + (1u << Rank)> insert(const BinomialHeap<N>& bh, const BinomialTree<Rank>& bt) {
    if constexpr (N == 0) {
        return { bt };
    } else if constexpr (Rank < std::countr_zero(N)) { // Rank of the first tree in the heap
        return { bt, bh };
    } else if constexpr (Rank == std::countr_zero(N)) {
        return insert(bh.ts, mergeTrees(bh.t, bt));
    } else {
        return { bh.t, insert(bh.ts, bt) };
    }
}

// Forward declaration
template <unsigned Rank, unsigned N, unsigned N1>
BinomialHeap<N + N1 + (1u << Rank)>
    mergeWithCarry(const BinomialTree<Rank>&, const BinomialHeap<N>&, const BinomialHeap<N1>&);

// Optimal* binomial heap merge.
// If the heaps correspond to lists of binomial trees, ordered by rank, this executes
// the classical merge algorithm, but combining same-ranked trees into one of the next rank.
template <unsigned N, unsigned N1>
BinomialHeap<N + N1> merge(const BinomialHeap<N>& lhs, const BinomialHeap<N1>& rhs) {
    if constexpr (N == 0) {
        return rhs;
    } else if constexpr (N1 == 0) {
        return lhs;
    } else {
        // The ranks of the heaps' first trees
        constexpr unsigned R = std::countr_zero(N);
        constexpr unsigned R1 = std::countr_zero(N1);
        if constexpr (R < R1) {
            return { lhs.t, merge(lhs.ts, rhs) };
        } else if constexpr (R > R1) {
            return { rhs.t, merge(rhs.ts, lhs) };
        } else {
            // There can be no two trees of the same rank in the result => merge into a tree
            // of the next rank. It corresponds to having a carry bit when adding N + N1.
            return mergeWithCarry(mergeTrees(lhs.t, rhs.t), lhs.ts, rhs.ts);
        }
    }
}

// Merges two binomial heaps while also adding a tree, formed by merging two same-ranked trees
// earlier during the merge. This corresponds to adding N+N1 while also having a "carry" bit at index Rank.
template <unsigned Rank, unsigned N, unsigned N1>
BinomialHeap<N + N1 + (1u << Rank)>
    mergeWithCarry(const BinomialTree<Rank>& carry, const BinomialHeap<N>& lhs, const BinomialHeap<N1>& rhs)
{
    if constexpr (N == 0) { // No more bits => just insert the accumulated carry into what's left from rhs
        return insert(rhs, carry);
    } else if constexpr (N1 == 0) { // Symmetrical case
        return insert(lhs, carry);
    } else if constexpr (Rank == std::countr_zero(N)) {
        // There are still >1 trees of the same rank => merge them & continue carrying over
        return mergeWithCarry(mergeTrees(carry, lhs.t), lhs.ts, rhs);
    } else if constexpr (Rank == std::countr_zero(N1)) { // Symmetrical case
        return mergeWithCarry(mergeTrees(carry, rhs.t), lhs, rhs.ts);
    } else { // Place carry bit here & continue ordinary merge
        return { carry, merge(lhs, rhs) };
    }
}

// Inserts a single value in a binomial heap by constructing a singleton heap & merging
template <unsigned N>
BinomialHeap<N + 1> insert(const BinomialHeap<N>& bh, const int value) {
    return merge(bh, BinomialHeap<1>{ { value } });
}

// Shorthand operator for inserting a value in a binomial heap.
template <unsigned N>
BinomialHeap<N + 1> operator+(const BinomialHeap<N>& bh, const int value) {
    return insert(bh, value);
}

// Convenience functions for heap construction.
// Builds a heap from a non-empty list of values (there's no such thing as an empty heap)
template <int... Xs>
BinomialHeap<sizeof...(Xs)> makeHeap() {
    return (BinomialHeap<0>{} + ... + Xs);
}

// Builds a heap from a non-empty integer sequence
template <int... Xs>
BinomialHeap<sizeof...(Xs)> makeHeap(std::integer_sequence<int, Xs...>) {
    return makeHeap<Xs...>();
}

// Builds a heap, containing the first N integers
template <int N>
BinomialHeap<N> makeHeapS() { return makeHeap(std::make_integer_sequence<int, N>{}); }
} // namespace Meta

// Stream output - for heap visualization.
// Example:
//  std::cout << Meta::makeHeap<2,5,3,4,6,1>() << "\n";
//  std::cout << Meta::makeHeapS<7>() << "\n";
template <unsigned Rank>
std::ostream& operator<<(std::ostream&, const Meta::BinomialTree<Rank>&);

template <unsigned Rank>
std::ostream& operator<<(std::ostream& os, const Meta::BinomialTreeArray<Rank>& bta) {
    if constexpr (Rank == 0) {
        return (os << bta.t);
    } else {
        return (os << bta.t << ' ' << bta.ts);
    }
}

template <unsigned Rank>
std::ostream& operator<<(std::ostream& os, const Meta::BinomialTree<Rank>& bt) {
    if constexpr (Rank == 0) {
        return (os << '[' << bt.value << ']');
    } else {
        return (os << '[' << bt.value << ' ' << bt.subtrees << ']');
    }
}

template <unsigned N>
std::ostream& operator<<(std::ostream& os, const Meta::BinomialHeap<N>& bh) {
    if constexpr (N == 0) {
        return os;
    } else {
        return (os << bh.t << ' ' << bh.ts);
    }
}
