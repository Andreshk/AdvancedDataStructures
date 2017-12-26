#pragma once
#include <iostream> // printing & debugging, not actually needed
#include <string>   // printing & debugging, not actually needed
#include <vector>
#include <limits>   // std::numeric_limits<>::max()
#include <array>
#include <algorithm>    // std::partition
#include <type_traits>  // std::conditional

// Recommended use: simple numbers or small, trivial structs
// (i.e. dates/timepoints) for T, and small K (~3-4)
template<class T, size_t K>
class kdtree
{
    using point = std::array<T, K>;
    using idx_t = std::conditional_t<sizeof(T) <= 4, uint32_t, uint64_t>;

    struct node
    {
        union
        {
            T splitVal;
            idx_t leftIdx;
        };
        // The last bit of this integer will be set for leaf nodes only.
        // This means that the real index will be formed from the rest bits, limiting
        // the number of nodes and value to 2^31 for small nodes and 2^63 otherwise.
        idx_t right;

        node(float splitVal) // non-leaf constructor, does not know the right index yet
            : splitVal{ splitVal }, right{ 0 } {}
        node(idx_t leftmostPt, idx_t right) // leaf constructor, knows it's a leaf
            : leftIdx{ leftmostPt }, right{ (right << 1) + 1 } {}

        idx_t rightIdx() const { return (right >> 1); }
        void setRight(idx_t r) { right = (r << 1); }
        bool isLeaf() const { return (right & 1); }
    };

    static const idx_t max_size = (std::numeric_limits<idx_t>::max() >> 1);
    static const idx_t maxKDDepth = 64;
    static const idx_t maxPtsInLeaf = 20;
    static const idx_t maxNaiveSortSize = 20;

    std::vector<node> nodes;
    std::vector<point> points;

    // insertion sort - used for small amounts of data, so practically O(1)
    static void sortNaive(T* const from, const idx_t n)
    {
        for (idx_t i = 1; i < n; ++i)
            for (idx_t j = i; j > 0 && from[j - 1] > from[j]; --j)
                std::swap(from[j - 1], from[j]);
    }

    // O(n) median-of-medians algorithm for finding a good-enough approximate median.
    // Warning: this not only reorders, but also overwrites parts of the array (!)
    static T findMedian(T* const from, idx_t n)
    {
        // find the medians-of-5 and reiterate over them until they are few enough
        while (n > maxNaiveSortSize)
        {
            const idx_t ndiv5 = n / 5, nmod5 = n % 5;
            // split into groups of 5 elements, find their medians naively
            // (by sorting) and then save them at the beginning of the array
            for (idx_t i = 0; i < ndiv5; ++i)
            {
                sortNaive(from + i * 5, 5); // these calls are hopefully inlined
                from[i] = from[i * 5 + 2];
            }
            // the last "group" may not be of size 5, but we need its median as well
            if (nmod5)
            {
                sortNaive(from + n - nmod5, nmod5);
                from[ndiv5] = from[n - (nmod5) / 2];
            }
            // calculate the new size
            n = ndiv5 + (nmod5 != 0);
        }
        // sort the remaining few elements naively and return their median
        sortNaive(from, n);
        return ((n % 2)
            ? from[n / 2]
            : (from[n / 2 - 1] + from[n / 2]) / T(2));
    }
public:
    kdtree(std::vector<point>&& pts)
    {
        const idx_t n = idx_t(pts.size());
        if (n >= max_size)
            throw std::range_error{ "Constructing kdtree with more than max_size elements!" };
        points = std::move(pts);
        // No need for recursion - we'll simulate it by keeping a
        // stack of small structs, corresponding to the real stack
        // frames. Keeping one stack of small structs instead of 4
        // stacks of integers and using uniform initialization &
        // structured bindings should help with code clarity and
        // shouldn't affect performance in this scenario (not tested).
        // Also - no need to name the structure in order to use it :D
        struct
        {
            size_t depth;
            idx_t from, to;
            idx_t parent; // where to "return" the value, i.e. index of the parent node
        } stack[maxKDDepth + 1];
        // The recursive build function will always return,
        // but the result may be discarded - so we disambiguate
        // by passing an invalid parent idx to the "function calls"
        // which results aren't needed, i.e. the left subtrees.
        const idx_t invalidIdx = max_size;
        stack[0] = { 1,0,n,invalidIdx };
        size_t stackSize = 1;
        // At every node we need to calculate some median values.
        // Therefore we allocate a buffer of the correct size once
        // and then fill and manipulate only parts of it.
        std::vector<T> buffer(n);
        while (stackSize)
        {
            // these MUST be bound by value, rather than reference (!)
            // otherwise the first push back to the stack will mess them up !
            const auto [depth,from,to,parent] = stack[--stackSize];
            const idx_t k = to - from;
            if (k < maxPtsInLeaf || depth >= maxKDDepth)
            {
                // create a node and return its index
                nodes.emplace_back(from, to);
                if (parent != invalidIdx)
                    nodes[parent].setRight(idx_t(nodes.size() - 1));
                continue; // and backtrack, of course
            }
            // Split the points by their median value, i.e.
            // fill the required part of the buffer...
            const size_t dim = (depth - 1) % K;
            for (idx_t i = from; i < to; ++i)
                buffer[i] = points[i][dim];
            // ...and calculate its median
            const T splitVal = findMedian(buffer.data() + from, k);
            // add a non-leaf node
            nodes.emplace_back(splitVal);
            const idx_t currIdx = idx_t(nodes.size() - 1);
            // tell my index to my parent the minute I know it
            if (parent != invalidIdx)
                nodes[parent].setRight(currIdx);
            // in case of multiple occurences of the median, the tree will be right-leaning
            // perhaps partition manually in case std::partition or the lambda isn't inlined
            const auto it = std::partition(points.begin() + from, points.begin() + to,
                [=](const point& pt) { return pt[dim] < splitVal; });
            idx_t mid = idx_t(std::distance(points.begin(), it));
            // we'll build the right subtree after the left, so it's pushed deeper in the stack
            stack[stackSize++] = { depth + 1, mid, to, currIdx };
            // no need to save the result of the recursive call for the left subtree
            stack[stackSize++] = { depth + 1, from, mid, invalidIdx };
        }
    }

    point nearestNeighbour(const point& pt) const
    {
        return {}; // to-do
    }

    std::vector<point> kNearestNeighbours(const point& pt) const
    {
        return {}; // to-do
    }

    std::vector<point> rangeSearch(const point& min, const point& max) const
    {
        return {}; // to-do
    }

    friend std::ostream& operator<<(std::ostream& os, const kdtree& kd)
    {
        os << "kdtree nodes: " << kd.nodes.size() << "\n"
           << "kdtree points: " << kd.points.size() << "\n";
        // Here the stack needs to contain only the indices
        // and depths of the nodes, waiting to be visited & printed.
        struct { idx_t idx; size_t depth; } stack[maxKDDepth + 1];
        // The first node visited will be the root, at index 0
        // and for this function, its depth will be 0.
        // (when building, the root's depth is 1)
        stack[0] = { idx_t(0), 0 };
        size_t stackSize = 1;
        while (stackSize)
        {
            // same note here for auto instead of auto&
            const auto [idx,depth] = stack[--stackSize];
            const node& nd = kd.nodes[idx];
            // we only keep the depths for pretty-printing
            const std::string padding(depth * 2, ' ');
            os << padding << idx << ": ";
            if (nd.isLeaf())
            {
                os << "#\n";
                // leaf node => print every point contained
                const idx_t from = nd.leftIdx, to = nd.rightIdx();
                for (idx_t i = from; i < to; ++i)
                {
                    const point& pt = kd.points[i];
                    os << padding << " [" << pt[0];
                    for (size_t j = 1; j < K; ++j) os << ',' << pt[j];
                    os << "]\n";
                }
                continue; // and backtrack up from the leaves
            }
            os << idx + 1 << ", " << nd.rightIdx() << " [";
            // we can determine the split axis by the node's depth
            if constexpr (K <= 3)
                os << char('X' + (depth % K));
            else
                os << depth % K;
            os << "] = " << nd.splitVal << "\n";
            // push the right node in the stack first, since we'll visit it later
            stack[stackSize++] = { nd.rightIdx(), depth + 1 };
            stack[stackSize++] = { idx + 1, depth + 1 };
        }
        return os << "\n";
    }
};