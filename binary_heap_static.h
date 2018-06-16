#pragma once
#include <vector>
#include <limits>      // std::numeric_limits::max
#include <cstddef>      // size_t
#include <algorithm>    // std::min, std::swap
#include <functional>   // std::less
#include <type_traits>  // std::is_nothrow_swappable
#include <utility>      // std::pair

template<class T, class Compare = std::less<T>>
class binary_heap_static
{
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
    binary_heap_static(const size_t numVertices,
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
    std::pair<T, vertex> extractMin();

    // The most complex operation for this type of heap, using the two-way referencing
    void decreaseKey(const vertex, const T&) noexcept(nothrow_comp);

    // More standard methods
    size_t size() const noexcept { return count; }
    bool  empty() const noexcept { return (size() == 0); }

    // Free all memory (!) and reinitialize for an updated number of vertices.
    // See the comment for the ctor.
    void reset(const size_t numVertices,
        vertex start,
        const T& zero = T{ 0 },
        const T& infinity = std::numeric_limits<T>::max(),
        const Compare& comp = Compare{});    
};

template<class T, class Compare>
size_t binary_heap_static<T, Compare>::minChildIdx(size_t idx) const noexcept(nothrow_comp)
{
    // Invariant: idx has at least one child
    const size_t left = leftChildIdx(idx);
    const size_t right = rightChildIdx(idx);
    if (right >= size() || comp(data[left], data[right]))
        return left;
    else
        return right;
}

template<class T, class Compare>
void binary_heap_static<T, Compare>::bubbleUp(size_t idx) noexcept(nothrow_comp_and_swap)
{
    while (idx) {
        size_t pIdx = parentIdx(idx);
        if (!comp(data[idx], data[pIdx]))
            return;
        swapNodes(idx, pIdx);
        idx = pIdx;
    }
}

template<class T, class Compare>
void binary_heap_static<T, Compare>::bubbleDown(size_t idx) noexcept(nothrow_comp_and_swap)
{
    while (leftChildIdx(idx) < size()) { // is leaf <=> no children
        size_t minIdx = minChildIdx(idx);
        if (comp(data[minIdx], data[idx])) {
            swapNodes(idx, minIdx);
            idx = minIdx;
        } else
            return;
    }
}

template<class T, class Compare>
void binary_heap_static<T, Compare>::swapNodes(const size_t idx1, const size_t idx2)
{
    using std::swap;
    swap(data[idx1].value, data[idx2].value);
    swap(data[idx1].v, data[idx2].v);
    swap(indices[idx1], indices[idx2]);
}

template<class T, class Compare>
auto binary_heap_static<T, Compare>::extractMin() -> std::pair<T, vertex>
{
    // Push the root node to the end of the array
    swapNodes(0, --count);
    // shrinking may be slow (implementation-specific) => just don't do it
    //data.pop_back();
    bubbleDown(0);
    return { data[count].value, indices[count] };
}

template<class T, class Compare>
void binary_heap_static<T, Compare>::decreaseKey(const vertex v, const T& newKey) noexcept(nothrow_comp)
{
    if (!comp(newKey, data[indices[v]].value))
        return;
    bubbleUp(indices[v]);
}

template<class T, class Compare>
void binary_heap_static<T, Compare>::reset(const size_t numVertices,
    vertex start, const T& zero, const T& infinity, const Compare& comp)
{
    this->comp = comp;
    count = numVertices;
    data.clear();
    indices.clear();
    if (numVertices == 0)
        return;
    // Initialize the vector as if all vertices have value = infinity
    data.assign(numVertices, pair{ infinity,0 });
    indices.assign(numVertices, 0);
    for (size_t i = 0; i < numVertices; ++i) {
        indices[i] = i;
        data[i].v = i;
    }
    // Manually "place" the starting vertex at the top of the heap
    data[start].value = zero;
    if (numVertices < 2)
        return;
    using std::swap;
    swap(data[start], data[0]);
    swap(indices[start], indices[0]);
}
