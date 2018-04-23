#pragma once
#include <vector>
#include <cstddef>      // size_t
#include <algorithm>    // std::min
#include <functional>   // std::less
#include <type_traits>  // std::is_nothrow_swappable

template<class T, size_t D, class Compare = std::less<T>>
class d_heap
{
    static_assert(D >= 2, "d-ary heaps must have degree D not less than 2!");
    static constexpr bool is_nothrow_comparable = true; // needs fixing!
    static constexpr bool nothrow_comp_and_swap = std::is_nothrow_swappable_v<T> && is_nothrow_comparable;

    std::vector<T> data;
    Compare comp;

    static size_t parentIdx(size_t idx) noexcept { return (idx - 1) / D; }
    static size_t leftmostChildIdx(size_t idx) noexcept { return D*idx + 1; }
    static size_t rightmostChildIdx(size_t idx) noexcept { return D*idx + D; }
    size_t minChildIdx(size_t idx) const noexcept(is_nothrow_comparable)
    {
        // Edge case handling (e.g. D == 2 or D == 4) may not lead to benefits...
        // Invariant: idx has at least one child
        size_t res = leftmostChildIdx(idx);
        // actual rightmost child
        const size_t r = std::min(rightmostChildIdx(idx), data.size() - 1);
        for (size_t i = res + 1; i <= r; i++)
            if (comp(data[i], data[res]))
                res = i;
        return res;
    }

    void bubbleUp() noexcept(nothrow_comp_and_swap)
    {
        using std::swap;
        size_t idx = data.size() - 1;
        while (idx)
        {
            size_t pIdx = parentIdx(idx);
            if (!comp(data[idx], data[pIdx]))
                return;
            swap(data[idx], data[pIdx]);
            idx = pIdx;
        }
    }
    void bubbleDown(size_t idx = 0) noexcept(nothrow_comp_and_swap)
    {
        using std::swap;
        while (leftmostChildIdx(idx) < data.size()) // is leaf <=> no children
        {
            size_t minIdx = minChildIdx(idx);
            if (comp(data[idx], data[minIdx]))
                return;
            swap(data[idx], data[minIdx]);
            idx = minIdx;
        }
    }

public:
    d_heap(const std::vector<T>& data = std::vector<T>{},
        const Compare& comp = Compare{})
        : data{ data }, comp{ comp }
    {
        // building the heap in linear time: call bubbleDown for all non-leaf indices
        const size_t n = size();
        if (n < 2)
            return;
        // finding the index of the first leaf is basically the same
        // as finding the largest k such that the sum d^0+d^1+...+d^k < n.
        size_t sum = 1, prevSum = 0, nextV = D;
        while (sum < n)
        {
            prevSum = sum;
            sum += nextV;
            nextV *= D;
        }
        const size_t firstLeaf = prevSum;
        for (size_t i = firstLeaf - 1; i < firstLeaf; i--) // we need i>=0
            bubbleDown(i);
    }

    const T&   top() const          { return data.front(); }
    bool     empty() const noexcept { return data.empty(); }
    size_t    size() const noexcept { return data.size(); }

    void push(const T& val)
    {
        emplace(val);
    }
    void push(T&& val)
    {
        emplace(std::move(val));
    }
    template<class... Args>
    void emplace(Args&&... args)
    {
        data.emplace_back(std::forward<Args>(args)...);
        bubbleUp();
    }

    void pop()
    {
        using std::swap;
        swap(data.front(), data.back());
        data.pop_back();
        bubbleDown();
    }

    void swap(d_heap& other) noexcept(noexcept(swap(data, other.data)) && noexcept(swap(comp, other.comp)))
    {
        using std::swap;
        swap(data, other.data);
        swap(comp, other.comp);
    }
};

template<class T, size_t D, class Compare>
void swap(d_heap<T, D, Compare>& lhs,
          d_heap<T, D, Compare>& rhs) noexcept(noexcept(lhs.swap(rhs)))
{
    lhs.swap(rhs);
}