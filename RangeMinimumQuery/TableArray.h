#pragma once
#include "TaggedArray.h"

// An array of flattened upper-triangular tables, residing in contiguous memory.
// Each table's entries represent the results of a given function for all i,j
// s.t. 0<=i<j<=n - in other words, [i;j) is a nonempty subrange of [0;n).
template <typename T, typename Idx = int, typename TableIdx = int>
class TableArray {
    DynamicArray<T, Idx> values; // All tables' values
    Idx n, tableSize; // Table rows & total value count in a table (precomputed from n)
    TableIdx numTables; // Table count
    // Returns the actual index in a table's values, where the result for [i;j) is stored.
    Idx getIndex(const Idx i, const Idx j) const {
        return (i * n + j - 1 - (i * (i + 1) / 2));
    }
public:
    TableArray() : values{}, n{ 0 }, tableSize{ 0 }, numTables{ 0 } {}
    TableArray(const Idx n, const TableIdx numTables)
        : n(n), tableSize(n * (n + 1) / 2), numTables(numTables)
    {
        values = DynamicArray<T, Idx>(int(numTables) * tableSize);
    }
    // Returns the results for range [i;j) in table idx
    T& at(const TableIdx idx, const Idx i, const Idx j) {
        vassert(0 <= idx && idx < numTables);
        vassert(0 <= i && i < j && j <= n);
        return values[int(idx) * tableSize + getIndex(i, j)];
    }
    // Returns the results for range [i;j) in table idx
    const T& at(const TableIdx idx, const Idx i, const Idx j) const {
        vassert(0 <= idx && idx < numTables);
        vassert(0 <= i && i < j && j <= n);
        return values[int(idx) * tableSize + getIndex(i,j)];
    }
};
