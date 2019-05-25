#pragma once
#include <vector>
#include <limits>      // std::numeric_limits::max
#include <cstddef>     // size_t
#include <algorithm>   // std::min, std::swap
#include <functional>  // std::less
#include <type_traits> // std::is_nothrow_swappable
#include <utility>     // std::pair

template <class T, class Compare = std::less<T>>
class BinaryHeapStatic {
    // Unfortunately false for std::less<T> for trivial types...
    static constexpr bool nothrow_comp
        = noexcept(std::declval<Compare>()(std::declval<const T&>(), std::declval<const T&>()));
    static constexpr bool nothrow_comp_and_swap
        = std::is_nothrow_swappable_v<T> && nothrow_comp;
    using vertex = size_t;
    struct pair { T value; vertex v; };

    std::vector<pair> data;
    std::vector<vertex> indices;
    Compare comp;
    size_t count;

    static size_t parentIdx(size_t idx) noexcept { return (idx - 1) / 2; }
    static size_t leftChildIdx(size_t idx) noexcept { return 2*idx + 1; }
    static size_t rightChildIdx(size_t idx) noexcept { return 2*idx + 2; }
    size_t minChildIdx(size_t idx) const noexcept(nothrow_comp);

    void bubbleUp(size_t idx) noexcept(nothrow_comp_and_swap);
    void bubbleDown(size_t idx) noexcept(nothrow_comp_and_swap);

    // Swap two values in the heap, keeping the two-way referencing
    void swapNodes(const size_t u, const size_t v);
public:
    // Reserves space for a given count of nodes (vertices) and prepares
    // the heap for Dijkstra's algorithm by inserting a value of infinity
    // for every node except the starting one, which has value zero.
    // If numVertices == 0, no memory is allocated, but operations
    // before the next call to reset() may lead to undefined behaviour.
    // Important postcondition: for every vertex v its value is data[indices[v]].
    // Requirement: comp(zero, infinity) == true && comp(zero, _) == true && comp(infinity, _) == false
    BinaryHeapStatic(
        const size_t numVertices,
        vertex start,
        const T& zero = T{ 0 },
        const T& infinity = std::numeric_limits<T>::max(),
        const Compare& comp = Compare{})
    {
        reset(numVertices, start, zero, infinity, comp);
    }

    // Standard operation; undefined behaviour for empty heaps
    const T& peek() const noexcept { return data[0].value; }

    // Also standard
    std::pair<vertex, T> extractMin();

    // The most complex operation for this type of heap, using the two-way referencing
    bool decreaseKey(const vertex, const T&) noexcept(nothrow_comp);

    // We can also keep track of the exact vertices currently in the heap
    bool contains(vertex) const noexcept;

    // More standard methods
    size_t size() const noexcept { return count; }
    bool  empty() const noexcept { return (size() == 0); }

    // Free all memory (!) and reinitialize for an updated number of vertices.
    // See the comment for the ctor.
    void reset(
        const size_t numVertices,
        vertex start,
        const T& zero = T{ 0 },
        const T& infinity = std::numeric_limits<T>::max(),
        const Compare& comp = Compare{});    
};

template <class T, class Compare>
size_t BinaryHeapStatic<T, Compare>::minChildIdx(size_t idx) const noexcept(nothrow_comp) {
    // Invariant: idx has at least one child
    const size_t left = leftChildIdx(idx);
    const size_t right = rightChildIdx(idx);
    if (right >= size() || comp(data[left].value, data[right].value)) {
        return left;
    } else {
        return right;
    }
}

template <class T, class Compare>
void BinaryHeapStatic<T, Compare>::bubbleUp(size_t idx) noexcept(nothrow_comp_and_swap) {
    while (idx) {
        size_t pIdx = parentIdx(idx);
        if (!comp(data[idx].value, data[pIdx].value)) {
            return;
        }
        swapNodes(idx, pIdx);
        idx = pIdx;
    }
}

template <class T, class Compare>
void BinaryHeapStatic<T, Compare>::bubbleDown(size_t idx) noexcept(nothrow_comp_and_swap) {
    while (leftChildIdx(idx) < size()) { // is leaf <=> no children
        size_t minIdx = minChildIdx(idx);
        if (comp(data[minIdx].value, data[idx].value)) {
            swapNodes(idx, minIdx);
            idx = minIdx;
        } else {
            return;
        }
    }
}

template <class T, class Compare>
void BinaryHeapStatic<T, Compare>::swapNodes(const size_t idx1, const size_t idx2) {
    using std::swap;
    swap(data[idx1], data[idx2]);
    swap(indices[data[idx1].v], indices[data[idx2].v]);
}

template <class T, class Compare>
auto BinaryHeapStatic<T, Compare>::extractMin() -> std::pair<vertex, T> {
    // Push the root node to the end of the array
    swapNodes(0, --count);
    // Shrinking the vectors may be slow (implementation-specific) => just don't do it
    bubbleDown(0);
    return { data[count].v, data[count].value };
}

template <class T, class Compare>
bool BinaryHeapStatic<T, Compare>::decreaseKey(const vertex v, const T& newKey) noexcept(nothrow_comp) {
    if (!comp(newKey, data[indices[v]].value)) {
        return false;
    }
    data[indices[v]].value = newKey;
    bubbleUp(indices[v]);
    return true;
}

template <class T, class Compare>
bool BinaryHeapStatic<T, Compare>::contains(vertex u) const noexcept {
    return (indices[u] < count);
}

template <class T, class Compare>
void BinaryHeapStatic<T, Compare>::reset(const size_t numVertices,
    vertex start, const T& zero, const T& infinity, const Compare& comp)
{
    this->comp = comp;
    count = numVertices;
    data.clear();
    indices.clear();
    if (numVertices == 0) {
        return;
    }
    // Initialize the vector as if all vertices have value = infinity
    data.assign(numVertices, pair{ infinity,0 });
    indices.assign(numVertices, 0);
    for (size_t i = 0; i < numVertices; ++i) {
        indices[i] = i;
        data[i].v = i;
    }
    // Manually "place" the starting vertex at the top of the heap
    data[start].value = zero;
    if (numVertices < 2) {
        return;
    }
    using std::swap;
    swap(data[start], data[0]);
    swap(indices[start], indices[0]);
}
