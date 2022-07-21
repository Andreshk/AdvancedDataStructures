#include <iostream> // for visualization purposes only
#include <algorithm> // std::min
#include <array>

template <unsigned Rank>
struct BinomialTree {
    int value = 0;
    // Subtrees of all lesser ranks, in decreasing orders.
    [[no_unique_address]] BinomialTree<int...(Rank:0:-1)> ...ts;

    // Note: getting .nontype_args...[0] in a pack expansion does not work (?) It's not a nested expansion, so why not?
    static constexpr unsigned rank() { return Rank; }
    bool valid() const { return (... && (value <= ts.value && ts.valid())); }
};

template <unsigned Rank>
BinomialTree<Rank+1> merge(const BinomialTree<Rank>& left, const BinomialTree<Rank>& right) {
    if (left.value < right.value) {
        return { left.value, right, left. ...ts ...};
    } else {
        return { right.value, left, right. ...ts ...};
    }
}

// This should take span<int, N> - why does Circle not have std::span???
template <unsigned Rank>
BinomialTree<Rank> makeTree(const int* values) {
    static constexpr unsigned N = (1u<<Rank);
    if constexpr (Rank == 0) {
        return { values[0] };
    } else {
        return merge(makeTree<Rank-1>(values),
                     makeTree<Rank-1>(&values[N/2]));
    }
}

template <unsigned N>
class BinomialHeap {
    // Holder for the trees' ranks
    static constexpr std::array Ranks = { int...(32).filter(N&(1u<<_0))... };
    [[no_unique_address]] BinomialTree<Ranks.[:]> ...ts;
public:
    // Splits the input array into chunks the size of each tree
    // and directly initializes the subtrees from these chunks.
    BinomialHeap(const std::array<int, N>& values)
        : ts{ makeTree<Ranks.[:]>(&values[N&((1u<<Ranks.[:])-1)]) }... {}
    int getMin() const { return (... std::min ts.value); } // lol
    bool valid() const { return (... && ts.valid()); }
};
// Helper deduction guide
template <unsigned N>
BinomialHeap(const std::array<int,N>&) -> BinomialHeap<N>;

// Stream output, nothing special
template <unsigned Rank>
std::ostream& operator<<(std::ostream& os, const BinomialTree<Rank>& t) {
    os << '[' << t.value;
    os << ' ' << t. ...ts ...;
    return (os << ']');
}
template <unsigned N>
std::ostream& operator<<(std::ostream& os, const BinomialHeap<N>& h) {
    os << h. ...ts << ' ' ...;
    return os;
}

int main() {
    int values[] = { 2,4,3,8,1,7,6,5 };
    const auto t = makeTree<3>(values);
    constexpr unsigned R = decltype(t)::rank();
    std::cout << "Ranks of BinomialTree<" << R << ">'s subtrees:";
    std::cout << ' ' << t.ts.rank() ...;
    std::cout << '\n';
    std::cout << "BinomialTree<" << R << ">: " << t << " (valid: " << std::boolalpha << t.valid() << ")\n";
    static_assert(sizeof(t) == (sizeof(t.value) << R)); // A tree of rank R has 2^R values.
    
    constexpr unsigned N = 13;
    std::array<int, N> values1 = { 5,13,9,1,7,11,6,3,4,10,2,12,8 };
    const BinomialHeap h{ values1 };
    std::cout << "Ranks of BinomialHeap<" << N << ">'s trees:";
    std::cout << ' ' << h.ts.rank() ...;
    std::cout <<'\n';
    std::cout << "BinomialHeap<" << N << ">: " << h
              << " (min=" << h.getMin() << "; valid: " << h.valid() << ")\n";
    static_assert(sizeof(h) == sizeof(t.value) * N);
}
