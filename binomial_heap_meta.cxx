#include <iostream> // for visualization purposes only
#include <algorithm> // std::min
#include <array>
#include <cassert>

// Circle uses libstdc++-10.3, where some C++20 libraries are still missing
// Barebones replacement for std::span<const int, N>
template <unsigned N>
struct Span {
    const int* ptr;
    Span(const int* ptr) : ptr{ptr} {}
    Span(const std::array<int, N>& data) : ptr{ &data[0] } {}
    template <unsigned Off, unsigned N1 = N - Off> requires (Off + N1 <= N)
    Span<N1> sub() const { return { ptr + Off }; }
    const int& operator[](size_t idx) const { assert(idx < N); return ptr[idx]; }
    static constexpr unsigned size() { return N; }
};
// More replacements: some <bit> functions - thank god GCC's builtins are constexpr
constexpr bool PowerOf2(unsigned n) { return (n > 0 && !(n&(n-1))); }
constexpr int Log2(unsigned n) { return __builtin_ctz(n); } // works for powers of 2 only!
static_assert(Log2(8) == 3 && PowerOf2(16));

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

template <unsigned N> requires (PowerOf2(N))
BinomialTree<Log2(N)> makeTree(Span<N> values) {
    static constexpr unsigned Rank = Log2(N);
    if constexpr (Rank == 0) {
        return { values[0] };
    } else {
        return merge(makeTree(values.template sub<0,N/2>()),
                     makeTree(values.template sub<N/2>()));
    }
}

template <unsigned N>
class BinomialHeap {
    // Holders for the trees' ranks and sizes
    static constexpr std::array Ranks = { int...(32).filter(N&(1u<<_0))... };
    static constexpr std::array Sizes = { (1u<<Ranks.[:])... };
    [[no_unique_address]] BinomialTree<Ranks.[:]> ...ts;
public:
    // Splits the input array into chunks the size of each tree
    // and directly initializes the subtrees from these chunks.
    BinomialHeap(Span<N> values)
        : ts{ makeTree(values.template sub<N % Sizes.[:], Sizes.[:]>()) }... {}
    int getMin() const { return (... std::min ts.value); } // lol
    bool valid() const { return (... && ts.valid()); }
};

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
    const Span values = std::array{ 2,4,3,8,1,7,6,5 };
    const auto t = makeTree(values);
    constexpr unsigned R = decltype(t)::rank();
    std::cout << "Ranks of BinomialTree<" << R << ">'s subtrees:";
    std::cout << ' ' << t.ts.rank() ...;
    std::cout << '\n';
    std::cout << "BinomialTree<" << R << ">: " << t << " (valid: " << std::boolalpha << t.valid() << ")\n";
    static_assert(sizeof(t) == (sizeof(t.value) << R)); // A tree of rank R has 2^R values.
    
    const Span values1 = std::array{ 5,13,9,1,7,11,6,3,4,10,2,12,8 };
    constexpr unsigned N = values1.size();
    const BinomialHeap h{ values1 };
    std::cout << "Ranks of BinomialHeap<" << N << ">'s trees:";
    for (int R : h.Ranks) { std::cout << ' ' << R; }
    std::cout << '\n';
    std::cout << "BinomialHeap<" << N << ">: " << h
              << " (min=" << h.getMin() << "; valid: " << h.valid() << ")\n";
    static_assert(sizeof(h) == sizeof(t.value) * N);
}
