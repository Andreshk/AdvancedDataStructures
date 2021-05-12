// Adapted from https://github.com/mikekazakov/eytzinger
#pragma once
#include <stdexcept>
#include <memory>
#include <new>
#include <vector>
#include <ranges>     // std::ranges::bidirectional_range
#include <utility>    // std::pair
#include <iterator>   // std::bidirectional_iterator
#include <algorithm>  // std::sort, std::unique
#include <functional> // std::less
#include <type_traits>
#include "vassert.h"  // can be replaced with <cassert>

// Helper function for throwing when needed (i.e. at())
[[noreturn]] void eytzingerThrow() { throw std::out_of_range("FixedEytzingerMap::at: key not found"); }

// Detects whether a type T contains an is_transparent type (and can be used for heterogenous comparison)
template <typename T>
concept IsTransparent = requires { typename T::is_transparent; };
// Detects whether an iterator can be used for construction of key-value pairs
template <typename It, typename Key, typename Value>
concept CanConstructPairsOf = std::input_iterator<It> && std::is_constructible_v<std::pair<Key, Value>, typename std::iterator_traits<It>::reference>;
// Conditionally adds a const modifier to a given type
template <bool B, typename T>
using AddConst = std::conditional_t<B, const T, T>;

template <typename Key, typename Value, class Compare = std::less<Key>>
class FixedEytzingerMap {
    // Proxy iterator - abstracts constness away to reduce code duplication between iterator & const_iterator
    template <bool isConst>
    struct IteratorImpl;
    // Helper iterator type, also & abstracting constness away
    template <bool isConst>
    struct PairPtrWrapper;
public:
    using size_type        = size_t;
    using value_type       = std::pair<Key, Value>; // Note: duplicated in CanConstructPairsOf concept
    using key_type         = Key;
    using mapped_type      = Value;
    using key_compare      = Compare;
    using iterator         = IteratorImpl<false>;
    using const_iterator   = IteratorImpl<true>;
    using range_pair       = std::pair<iterator, iterator>;
    using const_range_pair = std::pair<const_iterator, const_iterator>;

    static_assert(std::is_nothrow_move_constructible_v<key_type>);
    static_assert(std::is_nothrow_move_constructible_v<mapped_type>);
    static_assert(std::random_access_iterator<iterator>); // Bonus: usual set/map iterators are "just" bidirectional
    //static_assert(std::random_access_iterator<const_iterator>); // to-do
private:
    key_type* keys;
    mapped_type* values;
    size_type count_;
    [[no_unique_address]] Compare comp;

    // Allocate & deallocate uninitialized memory
    void allocateUninitialized(const size_t count);
    void deallocateUninitialized() noexcept;
    // Build the subtree, rooted in a given index from the values, starting from a given iterator.
    void buildTree(const size_t idx, std::vector<value_type>::iterator& it) noexcept;
    // Call all destructors (to be used before deallocation, f.e.)
    void destroyAll() const noexcept;
public:    
    // Construction
    FixedEytzingerMap();
    explicit FixedEytzingerMap(const Compare& comp);
    FixedEytzingerMap(const FixedEytzingerMap& other);
    FixedEytzingerMap(FixedEytzingerMap&& other);
    explicit FixedEytzingerMap(std::initializer_list<value_type> l, const Compare& comp = Compare());
    template <CanConstructPairsOf<Key, Value> It>
    FixedEytzingerMap(It from, It to, const Compare& comp = Compare());

    // Assignment
    FixedEytzingerMap& operator=(const FixedEytzingerMap& other);
    FixedEytzingerMap& operator=(FixedEytzingerMap&& other) noexcept;
    FixedEytzingerMap& operator=(std::initializer_list<value_type> l);
    template <CanConstructPairsOf<Key, Value> It>
    void assign(It begin, It end);
    void assign(std::initializer_list<value_type> l);
    // Destruction
    ~FixedEytzingerMap();

    // Element access
    mapped_type& at(const key_type& key);
    template <typename K2> requires IsTransparent<Compare>
    mapped_type& at(const K2& key);
    
    const mapped_type& at(const key_type& key) const;
    template <typename K2> requires IsTransparent<Compare>
    const mapped_type& at(const K2& key) const;

    mapped_type& operator[](const key_type& key) noexcept;
    template <typename K2> requires IsTransparent<Compare>
    mapped_type& operator[](const K2& key) noexcept;
    
    const mapped_type& operator[](const key_type& key) const noexcept;
    template <typename K2> requires IsTransparent<Compare>
    const mapped_type& operator[](const K2& key) const noexcept;
    
    // Iterators
    iterator       begin()  noexcept;
    iterator       end()    noexcept;
    const_iterator begin()  const noexcept;
    const_iterator end()    const noexcept;
    const_iterator cbegin() const noexcept;
    const_iterator cend()   const noexcept;
    
    // Modifiers
    void clear() noexcept;
    void swap(FixedEytzingerMap& other) noexcept;

    // Capacity
    bool empty() const noexcept;
    size_type size() const noexcept;
    size_type max_size() const noexcept;

    // Lookup
    size_type count(const key_type& key) const noexcept;
    template <typename K2> requires IsTransparent<Compare>
    size_type count(const K2& key) const noexcept;

    iterator find(const key_type& key) noexcept;
    template <typename K2> requires IsTransparent<Compare>
    iterator find(const K2& key) noexcept;
    
    const_iterator find(const key_type& key) const noexcept;
    template <typename K2> requires IsTransparent<Compare>
    const_iterator find(const K2& key) const noexcept;
    
    range_pair equal_range(const key_type& key) noexcept;
    template <typename K2> requires IsTransparent<Compare>
    range_pair equal_range(const K2& key) noexcept;

    const_range_pair equal_range(const key_type& key) const noexcept;
    template <typename K2> requires IsTransparent<Compare>
    const_range_pair equal_range(const K2& key) const noexcept;

    iterator lower_bound(const key_type& key) noexcept;
    template <typename K2> requires IsTransparent<Compare>
    iterator lower_bound(const K2& key) noexcept;
    
    const_iterator lower_bound(const key_type& key) const noexcept;
    template <typename K2> requires IsTransparent<Compare>
    const_iterator lower_bound(const K2& key) const noexcept;
    
    iterator upper_bound(const key_type& key) noexcept;
    template <typename K2> requires IsTransparent<Compare>
    iterator upper_bound(const K2& key) noexcept;
    
    const_iterator upper_bound(const key_type& key) const noexcept;
    template <typename K2> requires IsTransparent<Compare>
    const_iterator upper_bound(const K2& key) const noexcept;
};

template <typename Key, typename Value, typename Compare>
template <bool isConst>
struct FixedEytzingerMap<Key, Value, Compare>::IteratorImpl {
    using Value2 = AddConst<isConst, Value>; // The pointed-to value - const iterators contain a const pointer, non-const iterators a non-const one

    const Key *k;
    Value2 *v;
    friend FixedEytzingerMap<Key, Value, Compare>;

    IteratorImpl(const Key *k, Value2 *v) noexcept : k(k), v(v) {} // Only a map can construct iterators to itself
public:
    using iterator_category = std::contiguous_iterator_tag;
    using iterator_concept  = std::contiguous_iterator_tag;
    using difference_type   = ptrdiff_t;
    using value_type        = std::pair<Key, Value2>;
    using pointer           = PairPtrWrapper<isConst>;
    using reference         = std::pair<const Key&, Value2&>;

    IteratorImpl() noexcept : k(nullptr), v(nullptr) {}
    reference operator*() const noexcept { return { *k, *v }; }
    pointer operator->() const noexcept { return { k, v }; }
    reference operator[](difference_type n) const noexcept { return *(*this + n); }
    IteratorImpl& operator++() noexcept {
        ++k; ++v;
        return *this;
    }
    IteratorImpl operator++(int) noexcept {
        IteratorImpl res{ *this };
        ++(*this);
        return res;
    }
    IteratorImpl& operator--() noexcept {
        --k; --v;
        return *this;
    }
    IteratorImpl operator--(int) noexcept {
        IteratorImpl res{ *this };
        --(*this);
        return res;
    }
    IteratorImpl& operator+=(difference_type diff) noexcept {
        k += diff; v += diff;
        return *this;
    }
    IteratorImpl& operator-=(difference_type diff) noexcept {
        k -= diff; v -= diff;
        return *this;
    }
    IteratorImpl operator+(difference_type diff) const noexcept {
        return (IteratorImpl{ *this } += diff);
    }
    friend IteratorImpl operator+(difference_type diff, const IteratorImpl &it) noexcept {
        return (it + diff);
    }
    IteratorImpl operator-(difference_type diff) const noexcept {
        return (IteratorImpl{ *this } -= diff);
    }
    difference_type operator-(const IteratorImpl& rhs) const noexcept { return (k - rhs.k); }
    bool operator==(const IteratorImpl& rhs) const noexcept { return (k == rhs.k); }
    auto operator<=>(const IteratorImpl& rhs) const noexcept { return (k <=> rhs.k); }
};

template <typename Key, typename Value, typename Compare>
FixedEytzingerMap<Key, Value, Compare>::FixedEytzingerMap() : FixedEytzingerMap(Compare()) {}

template <typename Key, typename Value, typename Compare>
FixedEytzingerMap<Key, Value, Compare>::FixedEytzingerMap(const Compare& comp) :
    keys(nullptr),
    values(nullptr),
    count_(0),
    comp(comp)
{}

template <typename Key, typename Value, typename Compare>
FixedEytzingerMap<Key, Value, Compare>::FixedEytzingerMap(FixedEytzingerMap&& other) :
    keys(other.keys),
    values(other.values),
    count_(other.count_),
    comp(other.comp)
{
    other.keys = nullptr;
    other.values = nullptr;
    other.count_ = 0;
}

template <typename Key, typename Value, typename Compare>
FixedEytzingerMap<Key, Value, Compare>::FixedEytzingerMap(const FixedEytzingerMap& other) : FixedEytzingerMap(other.comp) {
    allocateUninitialized(other.count_);
    // If some copy constructor throws, the previously constructed keys/values will be destroyed safely
    Key *last_key = keys;
    Value *last_value = values;
    try {
        for (size_type i = 0; i < count_; ++i, ++last_key) {
            ::new((void*)(keys + i)) Key(other.keys[i]); // cast to void* to avoid suspicious overloads
        }
        for (size_type i = 0; i < count_; ++i, ++last_value) {
            ::new((void*)(values + i)) Value(other.values[i]);
        }
    } catch(...) {
        for (Key* it = keys; it < last_key; ++it) {
            it->~Key();
        }
        for (Value* it = values; it < last_value; ++it) {
            it->~Value();
        }
        deallocateUninitialized();
        std::rethrow_exception(std::current_exception());
    }
}

template <typename Key, typename Value, typename Compare>
FixedEytzingerMap<Key, Value, Compare>::FixedEytzingerMap(std::initializer_list<value_type> l, const Compare& comp) : FixedEytzingerMap(comp) {
    std::vector<value_type> t{ std::begin(l), std::end(l) };
    std::sort(t.begin(), t.end(), [this](const value_type& v1, const value_type& v2) {
        return this->comp(v1.first, v2.first);
    });
    t.erase(std::unique(t.begin(), t.end(), [this](const value_type& v1, const value_type& v2){
        return (!this->comp(v1.first, v2.first) && !this->comp(v2.first, v1.first));
    }), t.end());
    
    allocateUninitialized(t.size());
    auto it = t.begin();
    buildTree(0, it);
    vassert(it == t.end());
}

template <typename Key, typename Value, typename Compare>
template <CanConstructPairsOf<Key, Value> It>
FixedEytzingerMap<Key, Value, Compare>::FixedEytzingerMap(It from, It to, const Compare& comp) : FixedEytzingerMap(comp) {
    std::vector<value_type> t{ from, to };
    std::sort(std::begin(t), std::end(t), [this](const value_type& v1, const value_type& v2) {
        return this->comp(v1.first, v2.first);
    });
    t.erase(std::unique(t.begin(), t.end(), [this](const value_type& v1, const value_type& v2){
        return (!this->comp(v1.first, v2.first) && !this->comp(v2.first, v1.first));
    }), t.end());

    allocateUninitialized(t.size());
    auto it = t.begin();
    buildTree(0, it);
    vassert(it == t.end());
}

template <typename Key, typename Value, typename Compare>
FixedEytzingerMap<Key, Value, Compare>&
FixedEytzingerMap<Key, Value, Compare>::operator=(FixedEytzingerMap&& other) noexcept {
    if (this != &other) {
        clear();
        swap(other);
    }
    return *this;
}

template <typename Key, typename Value, typename Compare>
FixedEytzingerMap<Key, Value, Compare>&
FixedEytzingerMap<Key, Value, Compare>::operator=(const FixedEytzingerMap& other) {
    if (this != &other) {
        FixedEytzingerMap copy{ other };
        swap(copy);
    }
    return *this;
}

template <typename Key, typename Value, typename Compare>
FixedEytzingerMap<Key, Value, Compare>&
FixedEytzingerMap<Key, Value, Compare>::operator=(std::initializer_list<value_type> l) {
    FixedEytzingerMap copy{ l };
    swap(copy);
    return *this;
}

template <typename Key, typename Value, typename Compare>
template <CanConstructPairsOf<Key, Value> It>
void FixedEytzingerMap<Key, Value, Compare>::assign(It from, It to) {
    FixedEytzingerMap copy{ from, to };
    swap(copy);
}

template <typename Key, typename Value, typename Compare>
void FixedEytzingerMap<Key, Value, Compare>::assign(std::initializer_list<value_type> l) {
    FixedEytzingerMap copy{ l };
    swap(copy);
}

template <typename Key, typename Value, typename Compare>
FixedEytzingerMap<Key, Value, Compare>::~FixedEytzingerMap() {
    static_assert(std::ranges::random_access_range<FixedEytzingerMap>); // Bonus: usual set/map-s are "just" bidirectional
    destroyAll();
    deallocateUninitialized();
}

template <typename Key, typename Value, typename Compare>
void FixedEytzingerMap<Key, Value, Compare>::allocateUninitialized(const size_t count) {
    vassert(keys == nullptr && values == nullptr);
    count_ = count;
    try {
        // Allocate aligned, but uninitialzed memory - the values inside will be initialized in the special recursive order.
        // This way we won't make unnecessary initializations & move assignments
        keys = static_cast<Key*>(::operator new(count * sizeof(Key), std::align_val_t(alignof(Key))));
        values = static_cast<Value*>(::operator new(count * sizeof(Value), std::align_val_t(alignof(Value))));
    } catch (...) {
        deallocateUninitialized();
        std::rethrow_exception(std::current_exception());
    }
}

template <typename Key, typename Value, typename Compare>
void FixedEytzingerMap<Key, Value, Compare>::deallocateUninitialized() noexcept {
    if(keys) {
        // Elements should be destroyed beforehand (f.e. by destroyAll())
        // because this casts the pointer to void* & doesn't call destructors (!)
        ::operator delete(keys, std::align_val_t(alignof(Key)));
        keys = nullptr;
    }
    if(values) {
        ::operator delete(values, std::align_val_t(alignof(Value)));
        values = nullptr;
    }
    count_ = 0;
}

template <typename Key, typename Value, typename Compare>
void FixedEytzingerMap<Key, Value, Compare>::destroyAll() const noexcept {
    for (size_type i = 0; i < count_; ++i) {
        keys[i].~Key();
        values[i].~Value();
    }
}

template <typename Key, typename Value, typename Compare>
void FixedEytzingerMap<Key, Value, Compare>::buildTree(const size_t idx, std::vector<value_type>::iterator& it) noexcept {
    if (idx >= count_) {
        return;
    }
    // Left subtree
    buildTree(2 * idx + 1, it);
    // Root node (be it a leaf or not)
    ::new((void*)(keys + idx)) Key(std::move(it->first)); // cast to void* to avoid suspicious overloads
    ::new((void*)(values + idx)) Value(std::move(it->second)); // these obv rely on the memory being uninitialized
    ++it;
    // Right subtree
    buildTree(2 * idx + 2, it);
}

template <typename Key, typename Value, typename Compare>
void FixedEytzingerMap<Key, Value, Compare>::clear() noexcept {
    destroyAll();
    deallocateUninitialized();
}

template <typename Key, typename Value, typename Compare>
void FixedEytzingerMap<Key, Value, Compare>::swap(FixedEytzingerMap& other) noexcept {
    std::swap(keys, other.keys);
    std::swap(values, other.values);
    std::swap(count_, other.count_);
    std::swap(comp, other.comp);
}

template <typename Key, typename Value, typename Compare>
bool FixedEytzingerMap<Key,Value, Compare>::empty() const noexcept {
    return (count_ == 0);
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::size() const noexcept -> size_type {
    return count_;
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::max_size() const noexcept -> size_type {
    return (std::numeric_limits<size_type>::max() / 4);
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::begin() noexcept -> iterator {
    return { keys, values };
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::begin() const noexcept -> const_iterator {
    return { keys, values };
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::cbegin() const noexcept -> const_iterator {
    return begin();
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::end() noexcept -> iterator {
    return { keys + count_, values + count_ };
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::end() const noexcept -> const_iterator {
    return { keys + count_, values + count_ };
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::cend() const noexcept -> const_iterator {
    return end();
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::lower_bound(const Key& key) const noexcept -> const_iterator {
    size_type i = count_, j = 0;
    while (j < count_) {
        if (comp(keys[j], key)) {
            j = 2 * j + 2; // right branch
        } else {
            i = j;
            j = 2 * j + 1; // left branch
        }
    }
    return { keys + i, values + i };
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
auto FixedEytzingerMap<Key, Value, Compare>::lower_bound(const K2& key) const noexcept -> const_iterator {
    size_type i = count_, j = 0;
    while (j < count_) {
        if (comp(keys[j], key)) {
            j = 2 * j + 2; // right branch
        } else {
            i = j;
            j = 2 * j + 1; // left branch
        }
    }
    return { keys + i, values + i };
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::lower_bound(const Key& key) noexcept -> iterator {
    size_type i = count_, j = 0;
    while (j < count_) {
        if (comp(keys[j], key)) {
            j = 2 * j + 2; // right branch
        } else {
            i = j;
            j = 2 * j + 1; // left branch
        }
    }
    return { keys + i, values + i };
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
auto FixedEytzingerMap<Key, Value, Compare>::lower_bound(const K2& key) noexcept -> iterator {
    size_type i = count_, j = 0;
    while (j < count_) {
        if (comp(keys[j], key)) {
            j = 2 * j + 2; // right branch
        } else {
            i = j;
            j = 2 * j + 1; // left branch
        }
    }
    return { keys + i, values + i };
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::upper_bound(const key_type& key) noexcept -> iterator {
    size_type i = count_, j = 0;
    while (j < count_) {
        if (comp(key, keys[j])) {
            i = j;
            j = 2 * j + 1; // left branch
        }  else {
            j = 2 * j + 2; // right branch
        }
    }
    return { keys + i, values + i };
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
auto FixedEytzingerMap<Key, Value, Compare>::upper_bound(const K2& key) noexcept -> iterator {
    size_type i = count_, j = 0;
    while (j < count_) {
        if (comp(key, keys[j])) {
            i = j;
            j = 2 * j + 1; // left branch
        } else {
            j = 2 * j + 2; // right branch
        }
    }
    return { keys + i, values + i };
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::upper_bound(const key_type& key) const noexcept -> const_iterator {
    size_type i = count_, j = 0;
    while (j < count_) {
        if (comp(key, keys[j])) {
            i = j;
            j = 2 * j + 1; // left branch
        } else {
            j = 2 * j + 2; // right branch
        }
    }
    return { keys + i, values + i };
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
auto FixedEytzingerMap<Key, Value, Compare>::upper_bound(const K2& key) const noexcept -> const_iterator {
    size_type i = count_, j = 0;
    while (j < count_) {
        if (comp(key, keys[j])) {
            i = j;
            j = 2 * j + 1; // left branch
        } else {
            j = 2 * j + 2; // right branch
        }
    }
    return { keys + i, values + i };
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::find(const Key& key) const noexcept -> const_iterator {
    const_iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return res;
    }
    return end();
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
auto FixedEytzingerMap<Key, Value, Compare>::find(const K2& key) const noexcept -> const_iterator {
    const_iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return res;
    }
    return end();
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::find(const Key& key) noexcept -> iterator {
    iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return res;
    }
    return end();
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
auto FixedEytzingerMap<Key, Value, Compare>::find(const K2& key) noexcept -> iterator {
    iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return res;
    }
    return end();
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::equal_range(const Key& key) noexcept -> range_pair {
    iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return { res, std::next(res, 1) };
    }
    return { end(), end() };
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
auto FixedEytzingerMap<Key, Value, Compare>::equal_range(const K2& key) noexcept -> range_pair {
    iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return { res, std::next(res, 1) };
    }
    return { end(), end() };
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::equal_range(const Key& key) const noexcept -> const_range_pair {
    const_iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return { res, std::next(res, 1) };
    }
    return { end(), end() };
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
auto FixedEytzingerMap<Key, Value, Compare>::equal_range(const K2& key) const noexcept -> const_range_pair {
    const_iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return { res, std::next(res, 1) };
    }
    return { end(), end() };
}

template <typename Key, typename Value, typename Compare>
auto FixedEytzingerMap<Key, Value, Compare>::count(const key_type& key) const noexcept -> size_type {
    const_iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return 1;
    }
    return 0;
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
auto FixedEytzingerMap<Key, Value, Compare>::count(const K2& key) const noexcept -> size_type {
    const_iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return 1;
    }
    return 0;
}

template <typename Key, typename Value, typename Compare>
Value& FixedEytzingerMap<Key, Value, Compare>::at(const key_type& key) {
    iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return *res.v;
    }
    eytzingerThrow();
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
Value& FixedEytzingerMap<Key, Value, Compare>::at(const K2& key) {
    iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return *res.v;
    }
    eytzingerThrow();
}

template <typename Key, typename Value, typename Compare>
const Value& FixedEytzingerMap<Key, Value, Compare>::at(const key_type& key) const {
    const_iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return *res.v;
    }
    eytzingerThrow();
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
const Value& FixedEytzingerMap<Key, Value, Compare>::at(const K2& key) const {
    const_iterator res = lower_bound(key);
    if (res != end() && !comp(key, *res.k)) {
        return *res.v;
    }
    eytzingerThrow();
}

template <typename Key, typename Value, typename Compare>
Value& FixedEytzingerMap<Key, Value, Compare>::operator[](const key_type& key) noexcept {
    iterator res = lower_bound(key);
    vassert(res != end() && !comp(key, *res.k));
    return *res.v;
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
Value& FixedEytzingerMap<Key, Value, Compare>::operator[](const K2& key) noexcept {
    iterator res = lower_bound(key);
    vassert(res != end() && !comp(key, *res.k));
    return *res.v;
}

template <typename Key, typename Value, typename Compare>
const Value& FixedEytzingerMap<Key, Value, Compare>::operator[](const key_type& key) const noexcept {
    const_iterator res = lower_bound(key);
    vassert(res != end() && !comp(key, *res.k));
    return *res.v;
}

template <typename Key, typename Value, typename Compare>
template <typename K2>
requires IsTransparent<Compare>
const Value& FixedEytzingerMap<Key, Value, Compare>::operator[](const K2& key) const noexcept {
    const_iterator res = lower_bound(key);
    vassert(res != end() && !comp(key, *res.k));
    return *res.v;
}

template <typename Key, typename Value, typename Compare>
template <bool isConst>
struct FixedEytzingerMap<Key, Value, Compare>::PairPtrWrapper
    : std::pair<const Key&, AddConst<isConst, Value>&>
{
    using Value2 = AddConst<isConst, Value>;
    PairPtrWrapper(const Key* k, Value2* v) noexcept : std::pair<const Key&, Value2&>(*k, *v) {}
    const std::pair<const Key&, Value2&>* operator->() const noexcept { return this; }
};

template <typename Key, typename Value, typename Compare>
inline bool operator==(const FixedEytzingerMap<Key, Value, Compare>& lhs,
                       const FixedEytzingerMap<Key, Value, Compare>& rhs)
{
    return lhs.size() == rhs.size() && std::equal(lhs.begin(), lhs.end(), rhs.begin());
}

template <typename Key, typename Value, typename Compare>
inline bool operator!=(const FixedEytzingerMap<Key, Value, Compare>& lhs,
                       const FixedEytzingerMap<Key, Value, Compare>& rhs)
{
    return !(lhs == rhs);
}

namespace std
{
template <typename Key, typename Value, typename Compare>
inline void swap(FixedEytzingerMap<Key, Value, Compare>& lhs,
                 FixedEytzingerMap<Key, Value, Compare>& rhs)
{
    rhs.swap(lhs);
}
}
