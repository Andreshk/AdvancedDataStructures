#pragma once
#include <iostream> // for visualization
#include <bit> // std::countr_zero, std::has_single_bit

// Implementation of binomial heaps, parameterized by their size.
// Since template arguments should be resolved at compilation time,
// only heaps of a static size, known at compilation, can be constructed.
// Everything can therefore be constexpr, but specifiers are omitted for brevity.
// On a positive note, when the types contain all the information, the structure
// is as space efficient as possible - sizeof(BinomialHeap<N>) == N*sizeof(int).
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
template <unsigned N, bool = std::has_single_bit(N)>
struct BinomialHeap;

// The base case, therefore, is for a binomial heap whose size is a power of 2.
// It will compose of a single binomial tree of rank logN.
template <unsigned N>
struct BinomialHeap<N, true> {
    BinomialTree<std::countr_zero(N)> t;

    static constexpr unsigned Size = N;
    static_assert(Size == decltype(t)::Size);
    int getMin() const { return t.value; } // can be constexpr, like everything else
    bool valid() const { return t.valid(); }
};

template <unsigned N>
struct BinomialHeap<N, false> {
    BinomialTree<std::countr_zero(N)> t; // countr_zero returns the index of N's lowest bit
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
    if constexpr (Rank < std::countr_zero(N)) { // Rank of the first tree in the heap
        return { bt, bh };
    } else if constexpr (Rank > std::countr_zero(N)) {
        if constexpr (std::has_single_bit(N)) { // <=> BinomialHeap base case
            return { bh.t, {bt} };
        } else {
            return { bh.t, insert(bh.ts, bt) };
        }
    } else {
        if constexpr (std::has_single_bit(N)) { // <=> BinomialHeap base case
            return { mergeTrees(bh.t, bt) };
        } else {
            return insert(bh.ts, mergeTrees(bh.t, bt));
        }
    }
}

// Slow, unoptimal merging - inserts one heap's trees sequentially in the other.
// Essentially ignores the fact that the trees in rhs are ordered by rank.
template <unsigned N, unsigned N1>
BinomialHeap<N + N1> merge(const BinomialHeap<N>& lhs, const BinomialHeap<N1>& rhs) {
    if constexpr (std::has_single_bit(N1)) { // <=> BinomialHeap base case
        return insert(lhs, rhs.t);
    } else {
        return merge(insert(lhs, rhs.t), rhs.ts);
    }
}

// Inserts a single value in a binomial heap by constructing a singleton heap & merging
template <unsigned N>
BinomialHeap<N + 1> insert(const BinomialHeap<N>& bh, const int value) {
    return merge(bh, BinomialHeap<1>{ { value } });
}

// Builds a heap from a non-empty list of values (there's no such thing as an empty heap)
template <int X, int... Xs>
BinomialHeap<1 + sizeof...(Xs)> makeHeap() {
    if constexpr (sizeof...(Xs) == 0) {
        return { {X} };
    } else {
        return insert(makeHeap<Xs...>(), X);
    }
}
} // namespace Meta

// Stream output - for heap visualization.
// Example: std::cout << Meta::makeHeap<2,5,3,4,6,1>() << "\n";
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
    if constexpr (std::has_single_bit(N)) {
        return (os << bh.t);
    } else {
        return (os << bh.t << ' ' << bh.ts);
    }
}
