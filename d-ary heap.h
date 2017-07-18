#pragma once
#include <vector>
#include <cstddef>     // size_t
#include <functional>  // std::less

template<class T, size_t D, class Compare = std::less<T>>
class D_heap
{
    static_assert(D >= 2, "d-ary heaps must have degree D not less than 2!");

    std::vector<T> data;
    Compare comp;

    static size_t parentIdx(size_t idx) noexcept { return (idx - 1) / D; }
    static size_t leftmostChildIdx(size_t idx) noexcept { return D*idx + 1; }
    static size_t rightmostChildIdx(size_t idx) noexcept { return D*idx + D; }
    size_t minChildIdx(size_t idx) const noexcept
    {
        // unfortunately, one cannot partially specialize a single member function,
        // like making D_heap<T,2,...>::f behave differently than D_heap<T,3,...>::f
        // without writing separate and whole D_heap<T,2,...> and D_heap<T,3,...>
        // classes. Bummer.
        size_t res = leftmostChildIdx(idx);
        const size_t r = rightmostChildIdx(idx), n = data.size();
        for (size_t i = res + 1; i <= r && i < n; i++)
            if (comp(data[i], data[res]))
                res = i;
        return res;
    }

    // std::is_nothrow_swappable<T> is supported officially only since c++17
    void bubbleUp() //noexcept(std::is_nothrow_swappable<T>::value)
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
    void bubbleDown(size_t idx = 0) //noexcept(std::is_nothrow_swappable<T>::value)
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
    D_heap(const std::vector<T>& _data = std::vector<T>(),
               const Compare& _comp = Compare()) : data(_data), comp(_comp)
    {
        // building the heap in linear time: call bubbleDown for all non-leaf indices
        const size_t n = data.size();
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

    const T& top() const noexcept { return data.front(); }
    bool empty() const noexcept { return data.empty(); }
    size_t size() const noexcept { return data.size(); }

    void push(const T& val)
    {
        data.push_back(val);
        bubbleUp();
    }
    void push(T&& val)
    {
        data.push_back(std::move(val));
        bubbleUp();
    }
    template<class... Args>
    void emplace(Args&&... args)
    {
        data.emplace_back(std::forward<Args>(args)...);
        bubbleUp();
    }
    void pop()
    {
        std::swap(data.front(), data.back());
        data.pop_back();
        bubbleDown();
    }

    void swap(D_heap& other) noexcept(noexcept(swap(data, other.data)) && noexcept(swap(comp, other.comp)))
    {
        using std::swap;
        swap(data, other.data);
        swap(comp, other.comp);
    }
};

template<class T, size_t D, class Compare>
void swap(D_heap<T, D, Compare>& lhs,
          D_heap<T, D, Compare>& rhs) noexcept(noexcept(lhs.swap(rhs)))
{
    lhs.swap(rhs);
}