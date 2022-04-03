#pragma once
#include <ranges>
#include <algorithm> // std::ranges::min_element
#include <functional> // std::less
#include <bit>
#include "TaggedArray.h"
#include "vassert.h"
#include "IntTags.h"

// Returns floor(log2(n)), or the index of the highest set bit.
// Undefined for negative n
template <std::integral T>
constexpr T logn(const T n) {
    vassert(n >= 0);
    // Bit operations are defined on unsigned ints only, good thing this is a no-op
    using U = std::make_unsigned_t<T>;
    return (n == 0 ? 0 : std::bit_width(U(n)) - 1);
}
static_assert(logn(7) == 2 && logn(8) == 3 && logn(9) == 3);

// Data structure, supporting fast range minimum queries (RMQ-s) on a static set of values.
// Built in O(nlgn) time, uses O(nlgn) additional space & answers queries in O(1) time.
// Can (almost precisely) answer range minimum queries without needing to keep a copy of the values.
template <typename T, typename Idx = int, typename Comp = std::less<>>
class SparseTableRMQ {
    // The i-th level contains the indices of the minima in all subranges in values of length 2^(i+1).
    // These indices are stored in the subrange [offsets[i],offsets[i+1]) in the array.
    // This makes the total # of levels floor(lgn). Total memory is n(lgn-2)+lgn, more precisely.
    DynamicArray<Idx, Idx> indices;
    DynamicArray<Idx, Int<LevelIdx>> offsets;
    Comp comp;

    Idx minByVal(const ArrayView<const T, Idx>& values, const Idx i, const Idx j) const {
        return (comp(values[i], values[j]) ? i : j);
    }
    ArrayView<Idx, Idx> getLevel(const Int<LevelIdx> lvl) {
        return { &indices[offsets[lvl]], (offsets[lvl + 1] - offsets[lvl]) };
    }
    ArrayView<const Idx, Idx> getLevel(const Int<LevelIdx> lvl) const {
        return { &indices[offsets[lvl]], (offsets[lvl + 1] - offsets[lvl]) };
    }
public:
    SparseTableRMQ() = default;
    template <std::ranges::random_access_range Range>
        requires std::is_same_v<T, std::ranges::range_value_t<Range>>
    SparseTableRMQ(const Range& values_, const Comp& comp = Comp{}) : comp{comp} {
        const ArrayView<const T, Idx> values{ values_ };
        const Idx n = values.size();
        // The exact number of levels in the table
        const Int<LevelIdx> numLevels = Int<LevelIdx>(logn(int(n))); // Note: this cast to int is dangerous (!)
        // First calculate the offsets for each level
        offsets = DynamicArray<Idx, Int<LevelIdx>>(numLevels + 1);
        offsets[Int<LevelIdx>(0)] = Idx(0);
        for (Int<LevelIdx> lvl{ 0 }; lvl < numLevels; ++lvl) {
            const Idx r = (Idx(1) << int(lvl)); // the previous power of 2
            const Idx lvlSize = n - 2*r + 1; // exact level size
            offsets[lvl + 1] = offsets[lvl] + lvlSize;
        }
        // We now know the total indices count
        indices = DynamicArray<Idx, Idx>(offsets[numLevels]);
        // The first level is trivial...
        ArrayView<Idx, Idx> lvl0 = getLevel(Int<LevelIdx>(0));
        for (Idx i{ 0 }; i < n - 1; ++i) {
            lvl0[i] = minByVal(values, i, i+1);
        }
        // ... then we build up
        for (Int<LevelIdx> lvl{ 1 }; lvl < numLevels; ++lvl) {
            const Idx r = (Idx(1) << int(lvl)); // the previous power of 2
            const Idx lvlSize = n - 2*r + 1; // exact level size
            ArrayView<      Idx, Idx> curr = getLevel(lvl);
            ArrayView<const Idx, Idx> prev = getLevel(lvl - 1);
            for (Idx i{ 0 }; i < lvlSize; ++i) {
                curr[i] = minByVal(values, prev[i], prev[i + r]);
            }
        }
    }

    // RMQ can be "answered" with uncertainty if the original set of values isn't
    // available, by returning two indices such that one of them is guaranteed
    // to be the answer (which the callee can then compare in O(1) to check).
    // O(1) time complexity
    struct Pair { Idx a, b; };
    Pair argmin(const Idx i, const Idx j) const {
        vassert(i < j);
        if (j == i + 1) {
            return { i, i };
        }
        const int lvl = logn(int(j - i)); // Note: this cast to int is dangerous (!)
        const int n = (1 << lvl); // Biggest power of 2, <= (j-i)
        vassert(n <= (j - i) && 2*n > (j - i));
        ArrayView<const Idx, Idx> level = getLevel(Int<LevelIdx>(lvl) - 1);
        // Note - sometimes n == j-i, i.e. the range size is a power of 2 and these are the same indices
        return { level[i], level[j - n] };
    }

    // Answers an RMQ query precisely, given the original set of values.
    // O(1) time complexity.
    Idx argmin(const ArrayView<const T, Idx>& values, const Idx i, const Idx j) const {
        const auto [a, b] = argmin(i, j);
        return minByVal(values, a, b);
    }

    // Answers an RMQ query precisely, given the original set of values.
    // O(1) time complexity.
    const T& min(const ArrayView<const T, Idx>& values, const Idx i, const Idx j) const {
        return values[argmin(values, i, j)];
    }

    // Same as argmin, but naive and O(n) - to be used for testing only
    const Idx argmin_slow(const ArrayView<const T, Idx>& values, const Idx i, const Idx j) const {
        return *std::ranges::min_element(std::views::iota(i, j), comp, [&](const Idx idx) { return values[idx]; });
    }
    // Same as min, but naive and O(n) - to be used for testing only
    const T& min_slow(const ArrayView<const T, Idx>& values, const Idx i, const Idx j) const {
        return values[argmin_slow(values, i, j)];
    }
};
