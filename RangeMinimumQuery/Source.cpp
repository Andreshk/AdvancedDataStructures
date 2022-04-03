#include <fmt/core.h>
#include <fmt/ranges.h>
#include <limits>
#include <string>
#include <algorithm>
#include <string_view>

// Poor man's global #define - in the only .cpp, before all include-s
//#define DISABLE_TAGGED_INTS
#include "LinearRMQ.h"

struct Printer {
    // A cartesian tree does not keep a copy of the values it's built from,
    // so they should be provided for detailed printing.
    template <typename T, typename Comp, std::ranges::random_access_range Range>
    static void print(const CartesianTree<T, Comp>& t, const Range& values_) {
        if (t.nodes.empty()) {
            fmt::print("Empty tree.\n");
            return;
        }
        const ArrayView<const T, Int<ValueIdx>> values{ values_ };
        // Again keeping a local stack of nodes waiting to be
        // visited & printed. pad is used for pretty-printing.
        struct frame { Int<ValueIdx> idx; int pad; };
        std::vector<frame> stack;
        stack.emplace_back(t.rootIdx, 0);
        // This print() function is not supposed to be fast,
        // so we can afford some maybe-not-inlined stateful lambdas for code clarity...
        auto printVal = [&](Int<ValueIdx> idx) {
            if (idx == invalidIdx) {
                fmt::print("#");
            } else {
                fmt::print("{}", values[idx]);
            }
        };
        auto stackPush = [&](Int<ValueIdx> idx, int pad) {
            if (idx != invalidIdx) {
                stack.emplace_back(idx, pad);
            }
        };
        // until there are no nodes left, print:
        while (!stack.empty()) {
            // These should be bound by value, rather than reference (!)
            const auto [idx, pad] = stack.back();
            stack.pop_back();
            const Node& nd = t.nodes[idx];
            // Print the padding, corresponding to the node depth
            fmt::print("{:{}}{}-> ", "", pad, values[idx]);
            const Int<ValueIdx> leftIdx = nd.left, rightIdx = nd.right;
            // Print the values in the children nodes
            printVal(leftIdx);
            fmt::print(", ");
            printVal(rightIdx);
            fmt::print("\n");
            // The right child is pushed deeper in the stack,
            // since we want to visit the left subtree first.
            stackPush(rightIdx, pad + 2);
            stackPush(leftIdx, pad + 2);
        }
    }
};

template <std::ranges::random_access_range Range, typename Comp = std::less<>>
void testSparseTableRMQ(const Range& values_, const Comp& comp = Comp{}) {
    // This shouldn't output anything
    using T = std::ranges::range_value_t<Range>;
    const SparseTableRMQ<T, Int<ValueIdx>, Comp> test{ values_,comp };
    const ArrayView<const T, Int<ValueIdx>> values{ values_ };
    const Int<ValueIdx> n = values.size();
    for (Int<ValueIdx> i{ 0 }; i < n; ++i) {
        for (Int<ValueIdx> j = i + 1; j <= n; ++j) {
            // Note: comparing argmin may lead to false negatives since the
            // two algorithms may break ties between equal values differently.
            const T& result = test.min(values, i, j);
            const auto [a,b] = test.argmin(i, j);
            const T& result2 = values[comp(values[a], values[b]) ? a : b];
            const T& expected = test.min_slow(values, i, j);
            // check x!=y only using the supplied operator< (!)
            if (comp(result, expected) || comp(expected, result)
                || comp(result2, expected) || comp(expected, result2))
            {
                vassert(false);
                fmt::print("FAIL: (i,j)=({},{})\n", i, j);
                fmt::print("  res={} res2={} exp={}\n", result, result2, expected);
            }
        }
    }
}

void testTableArray(const std::vector<int>& values, const bool print = false) {
    // Example table usage: cache all RMQ results for a given array
    // in linear time (w/ respect to table size)
    const int n = int(values.size());
    TableArray<int> table{ n, 1 };
    for (int i = 0; i < n; ++i) {
        table.at(0, i, i + 1) = i;
    }
    // Make a recursive computation for a range [i;j)
    // based on the results for some subrange(s) of it.
    for (int d = 1; d < n; ++d) {
        for (int i = 0; i < (n - d); ++i) {
            const int j = i + d + 1;
            // We can use any [i;k) and [k;j), alternatively
            const int a = table.at(0, i, j - 1);
            const int b = table.at(0, i + 1, j);
            table.at(0, i, j) = (values[a] < values[b] ? a : b);
        }
    }
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j <= n; ++j) {
            const int result = table.at(0, i, j);
            const int expected = *std::ranges::min_element(std::views::iota(i, j), std::less<>{}, [&](const int i) { return values[i]; });
            vassert(result == expected);
        }
    }
    if (print) {
        fmt::print(" {}\n", fmt::join(values, " "));
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j <= i; ++j) {
                fmt::print(". ");
            }
            for (int j = i + 1; j <= n; ++j) {
                fmt::print("{} ", table.at(0, i, j));
            }
            fmt::print("\n");
        }
    }
}

template <std::ranges::random_access_range Range, typename Comp = std::less<>>
void testLinearRMQ(const Range& values, const Comp comp = Comp{}) {
    const int n = int(std::ranges::size(values));
    using T = std::ranges::range_value_t<Range>;
    const LinearRMQ<T, Comp> rmq{ values, comp };
    for (int i = 0; i < n; ++i) {
        for (int j = i + 1; j <= n; ++j) {
            const int result = rmq.argmin(i, j);
            const int expected = *std::ranges::min_element(std::views::iota(i, j), std::less<T>{}, [&](const int i) { return values[i]; });
            vassert(result == expected);
        }
    }
}

/* notes:
 * - add user-friendly aliases for Int64<Tag> = Int<Tag, int64_t>, etc.
 *   -> use the smallest possible values for Int<Chunk> & Int<BV>
 *   -> check whether the explicit conversions are ok
 * - consider compressing the bitvectors in a big one, too (!)
 *   -> this may remove the need to have small Int<BV>
 * - try serialization/deserialization in as small of a footprint as possible
 * - benchmarks, benchmarks, benchmarks
 * - add getMemUsage() methods to CartesianTree, RMQ01Info, SparseTableRMQ, TableArray, LinearRMQ
 */
int main() {
    const std::vector<int> values{ 8,7,2,2,8,6,9,4,5 };

    const CartesianTree ct{ values };
    Printer::print(ct, values);
    const RMQ01Info rmqInfo = ct.to01RMQ();
    fmt::print("    Values: {}\n", fmt::join(values, " "));
    fmt::print("    Depths: {}\n", fmt::join(rmqInfo.depths, " "));
    fmt::print("   Indices: {}\n", fmt::join(rmqInfo.indices, " "));
    fmt::print("Occurences: {}\n", fmt::join(rmqInfo.occurences, " "));
    fmt::print("\n");

    // Test simple RMQ
    testSparseTableRMQ(values);
    
    // Test RMQ for a non-trivial type w/ functor
    auto cmpByLength = [](const std::string_view& lhs, const std::string_view& rhs) {
        return (lhs.length() < rhs.length());
    };
    const std::vector<std::string_view> words{ "asdasdas", "foofoo", "xy", "xy", "12345678", "qwerty", "bigsad:((" };
    testSparseTableRMQ(words, cmpByLength);
    
    // Test RMQ w/ functor with capture
    auto cmpIndex = [values = ArrayView<const int, Int<ValueIdx>>(values)](const Int<ValueIdx> i, const Int<ValueIdx> j) {
        return (values[i] < values[j]);
    };
    testSparseTableRMQ(DynamicArray<Int<ValueIdx>>{ 2,1,5,6,3,8,7,4,0 }, cmpIndex);

    // Test the table (relies on the vector's bounds-checking)
    testTableArray({ 2,1,5,3,4 });

    // Test linear RMQ w/ a random permutation of [1..64]
    const std::vector<int> manyValues = { 43,18,19,38,28,9,12,11,30,58,41,44,15,
        46,6,29,17,56,53,59,1,40,25,47,31,23,32,21,10,54,50,34,8,27,13,55,16,63,
        7,64,4,3,36,26,24,20,51,49,37,62,48,2,60,35,52,61,33,42,14,22,57,39,5,45 };
    testLinearRMQ(manyValues);

    return 0;
}