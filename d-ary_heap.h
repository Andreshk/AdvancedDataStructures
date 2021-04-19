#pragma once
#include <vector>
#include <bit>          // std::bit_floor
#include <ranges>       // std::ranges::input_range
#include <cstddef>      // size_t
#include <algorithm>    // std::min
#include <functional>   // std::less
#include "vassert.h"

template<class T, size_t D, class Compare = std::less<T>>
    requires (D >= 2) // Tree degree cannot be 1 or 0...
class d_heap {
    std::vector<T> data;
    Compare comp;

    // Parent/children index manipulation.
    // Note: first & last make a half-closed interval, i.e. [first;last) (!)
    static size_t     parentIdx(const size_t idx) { return (idx - 1) / D; }
    static size_t firstChildIdx(const size_t idx) { return D*idx + 1; }
    static size_t  lastChildIdx(const size_t idx) { return D*idx + D; }
    // Find the index of the smallest child for a given node
    size_t minChildIdx(const size_t idx) const {
        size_t res = firstChildIdx(idx);
        vassert(res < data.size()); // given node should have at least one child
        if constexpr (D == 2) {
            // Only one other child to check for existence & optimality
            if (res + 1 < data.size() && comp(data[res + 1], data[res])) {
                return res + 1;
            } else {
                return res;
            }
        } else {
            // one after the rightmost existing child
            const size_t r = std::min(lastChildIdx(idx), data.size());
            for (size_t i = res + 1; i < r; i++) {
                if (comp(data[i], data[res])) {
                    res = i;
                }
            }
            return res;
        }
    }
    // Bubble up a value at the last index of the array
    void bubbleUp() {
        using std::swap;
        size_t idx = data.size() - 1;
        while (idx) {
            size_t pIdx = parentIdx(idx);
            if (!comp(data[idx], data[pIdx])) {
                return;
            }
            swap(data[idx], data[pIdx]);
            idx = pIdx;
        }
    }
    // Bubble down a value at a given index
    void bubbleDown(size_t idx = 0) {
        using std::swap;
        while (firstChildIdx(idx) < data.size()) { // is leaf <=> no children
            size_t minIdx = minChildIdx(idx);
            if (comp(data[idx], data[minIdx])) {
                return;
            }
            swap(data[idx], data[minIdx]);
            idx = minIdx;
        }
    }

    // Finding the index of the first leaf is basically the same
    // as finding the largest k such that the sum d^0+d^1+...+d^k < n.
    static size_t findFirstLeaf(const size_t n) {
        if constexpr (D == 2) {
            return (std::bit_floor(n) - 1);
        } else {
            size_t sum = 1, prevSum = 0, nextV = D;
            while (sum < n) {
                prevSum = sum;
                sum += nextV;
                nextV *= D;
            }
            return prevSum;
        }
    }

    // Constructs a valid heap from a possibly invalid (f.e. currently constructed)
    // one by calling bubbleDown for all non-leaf indices. This takes O(n) total time.
    void bubbleDownNonChildren() {
        const size_t n = size();
        if (n < 2) {
            return;
        }
        const size_t firstLeaf = findFirstLeaf(n);
        for (size_t i = firstLeaf - 1; i < firstLeaf; i--) { // we need i>=0, but stupid unsigned ints...
            bubbleDown(i);
        }
    }
public:
    template <std::ranges::input_range R>
    d_heap(R&& data = R{}, const Compare& comp = Compare{})
        : data{ std::forward<R>(data) }, comp{ comp }
    {
        bubbleDownNonChildren();
    }

    // Standard interface
    const T& top() const { return data.front(); }
    bool   empty() const { return data.empty(); }
    size_t  size() const { return data.size(); }

    // Insert a single value into the heap
    void push(const T& val) { emplace(val); }
    void push(T&& val) { emplace(std::move(val)); }
    template<class... Args>
    void emplace(Args&&... args) {
        data.emplace_back(std::forward<Args>(args)...);
        bubbleUp();
    }

    // Remove the smallest value from the heap
    // Note: doesn't return it, similar to std::priority_queue - use top() beforehand.
    void pop() {
        using std::swap;
        swap(data.front(), data.back());
        data.pop_back();
        bubbleDown();
    }

    // Attempts replacing the smallest element with one that's larger
    // than it & returns true on success. Note: no reallocations.
    bool tryReplaceTop(const T& newTop) {
        if (!comp(newTop, data.front())) {
            data.front() = newTop;
            bubbleDown();
            return true;
        } else {
            return false;
        }
    }
};