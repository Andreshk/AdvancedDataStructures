#pragma once
#include <ranges>
#include <algorithm> // std::ranges::min_element
#include <functional> // std::less
#include <type_traits>
#include "vassert.h"
#include "TaggedArray.h"
#include "LinearRMQOperators.h"
#include "CartesianTree.h"
#include "SparseTableRMQ.h"
#include "TableArray.h"

// Data structure, supporting fast range minimum queries (RMQ-s) on a static set of values.
// Built in O(n) time, uses only O(n) additional space & answers queries in O(1) time.
// Can answer range minimum queries without needing to keep a copy of the values.
// In short, performs a reduction from RMQ to LCA on a Cartesian tree to 0-1RMQ on the
// tree's Euler walk (counting all edges twice). This Euler walk is split into small chunks,
// for which all RMQ-s are precomputed in O(n) space total. The chunks' minima are stored
// in a more "naive" RMQ structure, also supporting O(1) queries and taking up O(n) space.
template <typename T, typename Comp = std::less<T>>
class LinearRMQ {
    // Size of the chunks, on which the 0-1RMQ will be split. Equal to lgn/2.
    // The number of chunks is therefore 2n/lgn = o(n).
    Int<ChunkSize> chunk;
    // 0-1RMQ data, collected via Cartesian tree traversal. O(n) (5n to be precise).
    RMQ01Info rmq01;
    // Each 0-1RMQ chunk is characterized by a bitvector of length chunk-1.
    // For each such bitvector we keep precoputed tables of the RMQ inside this chunk.
    // There are O(sqrt(n)) such vectors w/ O(lgn^2) data inside, making O(n) total.
    // All tables are concatenated in this array.
    TableArray<Int<Chunk>, Int<Chunk>, Int<BV>> tables;
    // Table of bitvectors, correspoding to each chunk. O(n) bits (!) total
    DynamicArray<Int<BV>, Int<ChunkIdx>> bitvectors;
    // Minimum depth value in each chunk. Currently kept as a separate copy.
    DynamicArray<Int<Depth>, Int<ChunkIdx>> chunkMinDepths;
    // The "naive", O(nlgn) RMQ built on top of the chunk depths array above.
    // Since the # of chunks is O(n/lgn), this actually takes O(n) space.
    SparseTableRMQ<Int<Depth>, Int<ChunkIdx>> chunksRMQ;

    // Compares two 0-1RMQ indices by the depths in the Cartesian tree.
    bool cmpDepths(const Int<RMQIdx>& lhs, const Int<RMQIdx>& rhs) const {
        return (rmq01.depths[lhs] < rmq01.depths[rhs]);
    };
public:
    template <std::ranges::random_access_range Range>
        requires std::is_same_v<T, std::ranges::range_value_t<Range>>
    LinearRMQ(const Range& values, const Comp& comp = Comp{}) {
        const Int<RMQIdx> n{ int(std::ranges::size(values)) };
        // In case n is too small. This also guarantees >0 bitvector length later.
        chunk = Int<ChunkSize>{ std::max(logn(int(n)) / 2, 2) };
        // Minimum number of chunks, enough to cover the RMQ <=> ceil((2*n-1) / chunk)
        const Int<ChunkIdx> numChunks = ((2 * n - 1) / chunk) + ((2 * n - 1) % chunk != 0);
        // Smallest n, generating a big enough RMQ for this number of chunks
        const Int<RMQIdx> n0 = (numChunks * chunk) / 2 + 1;
        // chunk may not divide 2*n-1 for a given n, so we want it as close as possible.
        vassert((2 * n0 - 1) % chunk <= 1);

        // The entire cartesian tree must be constructed before making the
        // 0-1RMQ reduction. Adding a single value to the tree may change
        // arbitrary many values in the 0-1RMQ sequence, not just the last 2 or 3.
        rmq01 = CartesianTree{ values, int(n0 - n), comp }.to01RMQ();
        vassert(rmq01.depths.size() == 2 * n0 - 1);
        // Precompute the RMQ of the depths for each bitvector, corresponding to a chunk.
        const Int<BV> numBVs{ 1 << (int(chunk) - 1) };
        tables = TableArray<Int<Chunk>, Int<Chunk>, Int<BV>>(toIndex(chunk), numBVs);
        bitvectors = DynamicArray<Int<BV>, Int<ChunkIdx>>(numChunks);
        DynamicArray<bool, Int<BV>> ready(numBVs);
        for (bool& b : ready) { b = false; }
        // Checks whether the precomputed table for a given bitvector was constructed correctly.
        auto validateTable = [&](const Int<BV> bv, const Int<RMQIdx> chunkStart) {
            if constexpr (vassertEnabled) { // Save some compilation time
                for (Int<Chunk> i{ 0 }; i < chunk; ++i) {
                    for (Int<Chunk> j = i + 1; j <= chunk; ++j) {
                        // These are indices, local to the chunks (!)
                        const Int<Chunk> result = tables.at(bv, i, j);
                        const Int<Chunk> expected = *std::ranges::min_element(std::views::iota(i, j),
                            std::less<>{}, [&](const Int<Chunk> k) { return rmq01.depths[chunkStart + k]; });
                        vassert(result < chunk&& expected < chunk);
                        // There may be duplicate depths in a given RMQ chunk (meaning result != expected),
                        // but it's guaranteed they refer to the same value in the original sequence.
                        vassert(rmq01.depths[chunkStart + result] == rmq01.depths[chunkStart + expected]);
                        vassert(rmq01.indices[chunkStart + result] == rmq01.indices[chunkStart + expected]);
                    }
                }
            }
        };
        auto generateTable = [&](const Int<ChunkIdx> idx, const Int<BV> bv) {
            // Fill the main diagonal naively
            for (Int<Chunk> i{ 0 }; i < chunk; ++i) {
                tables.at(bv, i, i + 1) = i;
            }
            const Int<RMQIdx> chunkStart = idx * chunk;
            // Build up, reusing the precalculated values for smaller intervals
            for (Int<Chunk> d{ 1 }; d < chunk; ++d) {
                for (Int<Chunk> i{ 0 }; i < (toIndex(chunk) - d); ++i) {
                    const Int<Chunk> j = i + d + 1;
                    // We can use any [i;k) and [k;j), alternatively
                    const Int<Chunk> a = tables.at(bv, i, j - 1);
                    const Int<Chunk> b = tables.at(bv, i + 1, j);
                    tables.at(bv, i, j) = (cmpDepths(chunkStart + a, chunkStart + b) ? a : b);
                }
            }
            if constexpr (vassertEnabled) {
                validateTable(bv, chunkStart);
            }
        };
        chunkMinDepths = DynamicArray<Int<Depth>, Int<ChunkIdx>>(numChunks);
        // Build the table to store the results for every chunk's bitvector
        // + each chunk's min in the vector in preparation for the per-chunk RMQ.
        // This is actually called the Four Russians' method (!)
        for (Int<ChunkIdx> i{ 0 }; i < numChunks; ++i) {
            const Int<RMQIdx> from = i * chunk;
            const Int<RMQIdx> to = (i + 1) * chunk;
            Int<BV> bv{ 0 };
            for (Int<Chunk> j{ 0 }; j < chunk - 1; ++j) {
                const Int<RMQIdx> curr = from + j;
                if (cmpDepths(curr, curr + 1)) {
                    bv |= (Int<BV>(1 << int(j)));
                }
            }
            vassert(bv < numBVs && "Bitvector too large!");
            if (!ready[bv]) {
                generateTable(i, bv);
                ready[bv] = true;
            }
            bitvectors[i] = bv;
            const Int<Chunk> chunkMin = tables.at(bv, Int<Chunk>(0), toIndex(chunk));
            chunkMinDepths[i] = rmq01.depths[from + chunkMin];
        }
        // We can now create the "naive" RMQ table pretty easily - we actually
        // do want to find the minimum depth & to which chunk it belongs.
        chunksRMQ = SparseTableRMQ<Int<Depth>, Int<ChunkIdx>>{ chunkMinDepths };
    }
    // Forbid creating from temporaries
    template <std::ranges::random_access_range Range>
    LinearRMQ(Range&&, const Comp& = Comp{}) = delete;

    // Returns the index of the minimum value in the range [i;j) of values,
    // from which this object was built without having access to the values (!)
    int argmin(const int i, const int j) const {
        vassert(i < j);
        if (i + 1 == j) {
            return i;
        }
        // RMQ works for a closed range, so we convert from [i;j) to [i;j-1]
        // and remap to a closed range in the 0-1RMQ.
        Int<RMQIdx> rmqFrom = rmq01.occurences[Int<ValueIdx>(i)];
        Int<RMQIdx> rmqTo   = rmq01.occurences[Int<ValueIdx>(j - 1)];
        if (rmqFrom > rmqTo) {
            std::swap(rmqFrom, rmqTo);
        }
        // We are actually looking for the RMQIdx that gives a minimum depth in this range.
        // First we find in which chunks the two inclusive (!) bounds lie.
        const Int<ChunkIdx> chunkFrom = (rmqFrom / chunk);
        const Int<ChunkIdx> chunkTo = (rmqTo / chunk);
        const Int<Chunk> sliceFrom = (rmqFrom % chunk); // Lower bound, in [0;chunk)
        const Int<Chunk> sliceTo = (rmqTo % chunk) + 1; // Upper bound, in (1;chunk] (!) Not the same as ((rmqTo+1)%chunk)
        // Whether the range ends split the chunks at the ends in two
        const bool needsLeft = (sliceFrom != 0);
        const bool needsRight = (sliceTo != toIndex(chunk));
        Int<RMQIdx> res{ -1 };
        if (chunkFrom == chunkTo) {
            // [rmqFrom;rmqTo] lies in a single chunk => just access its bitvector
            const Int<BV> bv = bitvectors[chunkFrom];
            const Int<Chunk> sliceMin = tables.at(bv, sliceFrom, sliceTo);
            const Int<RMQIdx> sliceRes = (chunkFrom * chunk) + sliceMin;
            res = sliceRes;
        } else {
            if ((chunkFrom + needsLeft) < (chunkTo + !needsRight)) {
                // There is at least one full block => apply chunk-wise "naive" RMQ
                const Int<ChunkIdx> minChunkIdx = chunksRMQ.argmin(chunkMinDepths, chunkFrom + needsLeft, chunkTo + !needsRight);
                const Int<BV> bv = bitvectors[minChunkIdx];
                const Int<Chunk> sliceMin = tables.at(bv, Int<Chunk>(0), toIndex(chunk));
                res = (minChunkIdx * chunk) + sliceMin;
            }
            if (needsLeft) {
                // Get the chunk's bitvector & find the min for the slice in its table
                const Int<BV> bv = bitvectors[chunkFrom];
                const Int<Chunk> sliceMin = tables.at(bv, sliceFrom, toIndex(chunk));
                // Don't forget to offset with the chunk's start
                const Int<RMQIdx> sliceRes = (chunkFrom * chunk) + sliceMin;
                if (res == -1 || cmpDepths(sliceRes, res)) {
                    res = sliceRes;
                }
            }
            if (needsRight) {
                // The same, but for the last incomplete chunk
                const Int<BV> bv = bitvectors[chunkTo];
                const Int<Chunk> sliceMin = tables.at(bv, Int<Chunk>(0), sliceTo);
                const Int<RMQIdx> sliceRes = (chunkTo * chunk) + sliceMin;
                if (res == -1 || cmpDepths(sliceRes, res)) {
                    res = sliceRes;
                }
            }
        }
        vassert(res != -1 && "At least one of the cases should be valid!");
        return int(rmq01.indices[res]);
    }

    // Given the same range of values, from which this object
    // was built, returns the minimum value in the range [i;j).
    template <std::ranges::random_access_range Range>
        requires std::is_same_v<T, std::ranges::range_value_t<Range>>
    const T& min(const Range& values, const int i, const int j) const {
        return values[argmin(i, j)];
    }
};

// Deduction guide
template <std::ranges::random_access_range Range, typename Comp = std::less<std::ranges::range_value_t<Range>>>
LinearRMQ(const Range&, const Comp& = Comp{}) -> LinearRMQ<std::ranges::range_value_t<Range>, Comp>;

// Make sure at compile-time that creating LinearRMQ-s from temporaries is impossible.
template <typename T, typename = void>
inline constexpr bool canConstructFromTemp = false;
template <std::ranges::random_access_range T>
inline constexpr bool canConstructFromTemp<T, std::void_t<decltype(LinearRMQ(std::declval<T>()))>> = true;

static_assert(!canConstructFromTemp<DynamicArray<float>>);
