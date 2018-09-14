#pragma once
#include <iostream> // printing & debugging purposes
#include <array>
#include <vector>
#include <cstdint>  // uint{32,64}_t
#include <cstddef>  // size_t
#include <optional> // std::optional
#include <type_traits>   // std::is_unsigned
#include <unordered_map> // will be replaced with our no-collision hashing

// The helper mathematical & bitwise-operation functions can be found at the bottom of this file.

template <typename value_t = uint64_t>
class x_fast_trie {
    static_assert(std::is_unsigned<value_t>::value,
        "Willard's algorithm works only on unsigned integral types!");

    // This is allowed to have an arbitrary underlying type and will not be
    // intermixed with value_t in the code - they simply have different semantics.
    // Of course, this type limits the number of values contained.
    using idx_t = uint32_t;
    // Code cleanliness -> code godliness
    using pair_t = std::array<idx_t, 2>;
    using hashmap_t = std::unordered_map<value_t, pair_t>;
    // Placeholder function, will be replaced with a construction of our custom no-collision hashmap
    static hashmap_t make_hash(const std::vector<value_t>& level, const std::vector<idx_t> nodes) {
        hashmap_t hash;
        for (size_t i = 0; i < level.size(); ++i)
            hash.emplace(level[i], pair_t{ nodes[i], invalid_idx });
        return hash;
    }
    // For code clarity, expected to get inlined away (and also get replaced in our custom hashmap)
    static bool exists_in(const hashmap_t& hash, value_t val) {
        return (hash.find(val) != hash.end());
    }

    // Every node contains the indices of the values, corresponding
    // to its left- and rightmost leaf in the temporary trie.
    static constexpr idx_t root_idx = 0;
    static constexpr idx_t invalid_idx = ~idx_t{ 0 };
    static constexpr idx_t max_size = invalid_idx / 2;
    static bool is_invalid(idx_t idx) { return (idx == invalid_idx); }
    struct node {
        idx_t children[2];
        node() : children{ invalid_idx,invalid_idx } {};
    };

    std::vector<value_t> values;   // the values contained, in sorted order
    std::vector<hashmap_t> levels; // for every trie level, a hashmap to pairs of leaf indices
    const size_t bits;             // = ceil(log2(U)), where U is the maximum in values

    // Insert a value in the temporary trie, represented by the nodes array
    void trie_insert(std::vector<node>& nodes, const value_t val) {
        // Reverse the "significant" bits for easier access
        value_t rval = reverse_bits(val, bits);
        size_t rest_bits = bits;
        idx_t curr_idx = 0;
        // Navigate common prefix in tree
        while (true) {
            idx_t child_idx = nodes[curr_idx].children[rval & 1];
            if (is_invalid(child_idx))
                break;
            curr_idx = child_idx;
            rval >>= 1;
            --rest_bits;
        }
        // Add the rest of the bits
        while (rest_bits) {
            nodes.emplace_back(); // an empty node
            idx_t last_idx = idx_t(nodes.size() - 1);
            nodes[curr_idx].children[rval & 1] = last_idx;
            curr_idx = last_idx;
            rval >>= 1;
            --rest_bits;
        }
    }

    // Updates a level in the trie, using previously-allocated buffers for calculations
    void update_level(const std::vector<node>& nodes
        , std::vector<value_t>& curr_level, std::vector<idx_t>& curr_nodes
        , std::vector<value_t>& level_buffer, std::vector<idx_t>& nodes_buffer) {
        for (size_t i = 0; i < curr_nodes.size(); ++i) {
            for (size_t pos = 0; pos < 2; ++pos) { // unroll this pls
                idx_t child_idx = nodes[curr_nodes[i]].children[pos];
                if (!is_invalid(child_idx)) {
                    level_buffer.push_back(2 * curr_level[i] + pos);
                    nodes_buffer.push_back(child_idx);
                }
            }
        }
        std::swap(curr_nodes, nodes_buffer);
        std::swap(curr_level, level_buffer);
        // Fun fact: clear() almost always keeps the vector capacity,
        // so level updating should cause no reallocations.
        nodes_buffer.clear();
        level_buffer.clear();
    }
    
    // The crux of Willard's algorithm
    std::optional<value_t> find(value_t x, bool succ) const {
        if (empty())
            return {};
        using result_t = std::optional<value_t>;
        // Do not search for values outside of the range
        if (x > values.back())
            return (succ ? result_t{} : result_t{ values.back() });
        if (x < values.front())
            return (succ ? result_t{ values.front() } : result_t{});
        // This also catches the corner cases of the last value's
        // successor and the first value's predeccessor.
        if (exists_in(levels.back(), x))
            return { x };
        // Find the longest matching prefix of the searched value
        // by binary searching through the trie levels
        size_t l = 0, r = bits;
        while (r - l > 1) {
            const size_t m = (r + l) / 2;
            const size_t i = bits - m;
            if (exists_in(levels[m], x >> i))
                l = m;
            else
                r = m;
        }
        // xsplit == kappa(longest matching prefix)
        const value_t xsplit = x >> (bits - l);
        // The contents of the "splitting" node, i.e. its value indices
        const pair_t& arr = levels[l].at(xsplit);
        // Now do some more magic to save a couple of branches...
        static const int index[] = { -1,1 };
        // For starters, we check where this node has a left or a right
        // child node (we know there is exactly one) by another hashmap lookup
        if (exists_in(levels[l + 1], 2 * xsplit + succ))
            return { values[arr[!succ]] };
        else
            return { values[arr[succ] + index[succ]] };
    }
public:
    // Here be magic
    x_fast_trie(const std::vector<value_t>& _values) : bits{ max_bit_length(_values) } {
        // Very, very unlikely...
        if (_values.empty() || _values.size() >= max_size)
            return;
        const size_t n = _values.size();
        // The final node count is expected to be around 2n. The trie is
        // actually a temporary structure, needed only at initialization!
        std::vector<node> nodes{ 1,node{} }; // there's always a root node
        nodes.reserve(2 * n);
        // First build the trie...
        for (auto val : _values)
            trie_insert(nodes, val);
        // ...then save its levels in an array of hashmaps.
        levels.reserve(bits + 1);
        // Level by level, including the root and the leaves...
        // Invariant: at any iteration curr_level.size() == curr_nodes.size()
        // curr_level contains prefixes and curr_nodes the indices
        // of the respective nodes. Buffers will be used as buffers.
        std::vector<value_t> curr_level{ 0 }, level_buffer;
        std::vector<idx_t> curr_nodes{ root_idx }, nodes_buffer;
        level_buffer.reserve(n);
        nodes_buffer.reserve(n);
        for (size_t b = 0; b < bits; ++b) {
            // In the beginning, the hashmaps map a prefix value to its trie node
            levels.push_back(make_hash(curr_level, curr_nodes));
            update_level(nodes, curr_level, curr_nodes, level_buffer, nodes_buffer);
        }
        // Unfortunately, we also need the last level in a hashmap for quick lookup
        levels.push_back(make_hash(curr_level, curr_nodes));
        // Now all indices in curr_nodes point to the leaves, in sorted order...
        values = curr_level;
        for (size_t i = 0; i < curr_nodes.size(); ++i) {
            node& node = nodes[curr_nodes[i]];
            node.children[0] = idx_t(i);
            node.children[1] = idx_t(i);
        }
        // Traverse the tree backwards, updating the value indices
        // Until now, each node contains the indices of its two child nodes
        for (size_t b = levels.size() - 2; b + 1 >= 1; --b) // no b>=0 check for unsigned b...
            for (const auto& p : levels[b]) {
                node& node = nodes[p.second[0]];
                // Child node indices (will be equal when node has an only child)
                const idx_t left_idx = node.children[is_invalid(node.children[0])];
                const idx_t right_idx = node.children[!is_invalid(node.children[1])];
                node.children[0] = nodes[left_idx].children[0];
                node.children[1] = nodes[right_idx].children[1];
            }
        // Now every node contains the indices of the values, corresponding to
        // its left- and rightmost leaves, but the hashmaps still contain node indices.
        // So we replace each node's index with its contents and discard the trie itself.
        for (auto& hm : levels)
            for (auto& p : hm) { // structured bindings do not work here
                pair_t& arr = p.second;
                const node& node = nodes[arr[0]];
                arr[0] = node.children[0];
                arr[1] = node.children[1];
            }
    }

    // Finding the previous and next value are dual operations, after all.
    std::optional<value_t> pred(value_t x) const { return find(x, false); }
    std::optional<value_t> succ(value_t x) const { return find(x, true); }

    // Standard...
    size_t size() const noexcept { return values.size(); }
    bool empty() const noexcept { return values.empty(); }

    // Prints detailed trie information, incl. level hashmaps
    // and whether the leaves point to the correct values
    void print() const {
        std::cout << "Bits: " << bits << "\nValue count: " << size() << "\n";
        if (size() > 50)
            return;
        std::cout << "\nValues (sorted):\n";
        for (auto x : values) { print_bits(x, bits); std::cout << "\n"; }
        std::cout << "\nHashes:\n";
        for (size_t i = 0; i < levels.size(); ++i) {
            std::cout << "  Level " << i << "\n";
            for (const auto& [val, p] : levels[i]) {
                std::cout << "val: "; print_bits(val, i);
                std::cout << " {"; print_bits(values[p[0]], bits);
                std::cout << ",";  print_bits(values[p[1]], bits);
                std::cout << "}\n";
            }
        }
    }
};

namespace { // Utilities
    // Printing & debugging
    template <typename value_t>
    void print_bits(value_t val, size_t bits) {
        std::cout << val << '[';
        for (size_t i = bits - 1; i < bits; --i)
            std::cout << ((val >> i) & 1 ? '1' : '0');
        std::cout << ']';
    }
    // These two beg for vectorization
    template <typename value_t>
    size_t bit_length(value_t val) {
        size_t result = 0;
        while (val) {
            val >>= 1;
            ++result;
        }
        return result;
    }
    template <typename value_t>
    size_t max_bit_length(const std::vector<value_t>& values) {
        size_t result = 1;
        for (auto x : values) {
            size_t curr = bit_length(x);
            if (result < curr)
                result = curr;
        }
        return result;
    }

    // There are other ways to flip bits, hopefully this one isn't too slow
    uint8_t reverse_bits(uint8_t val, size_t bits) {
        val = ((val >> 1) & 0x55ui8) | ((val & 0x55ui8) << 1);
        val = ((val >> 2) & 0x33ui8) | ((val & 0x33ui8) << 2);
        val = (val >> 4) | (val << 4);
        return (val >> (8 - bits));
    }
    uint16_t reverse_bits(uint16_t val, size_t bits) {
        val = ((val >> 1) & 0x5555ui16) | ((val & 0x5555ui16) << 1);
        val = ((val >> 2) & 0x3333ui16) | ((val & 0x3333ui16) << 2);
        val = ((val >> 4) & 0x0F0Fui16) | ((val & 0x0F0Fui16) << 4);
        val = (val >> 8) | (val << 8);
        return (val >> (16 - bits));
    }
    uint32_t reverse_bits(uint32_t val, size_t bits) {
        val = ((val >> 1) & 0x5555'5555ui32) | ((val & 0x5555'5555ui32) << 1);
        val = ((val >> 2) & 0x3333'3333ui32) | ((val & 0x3333'3333ui32) << 2);
        val = ((val >> 4) & 0x0F0F'0F0Fui32) | ((val & 0x0F0F'0F0Fui32) << 4);
        val = ((val >> 8) & 0x00FF'00FFui32) | ((val & 0x00FF'00FFui32) << 8);
        val = (val >> 16) | (val << 16);
        return (val >> (32 - bits));
    }
    uint64_t reverse_bits(uint64_t val, size_t bits) {
        val = ((val >> 1) & 0x5555'5555'5555'5555ui64) | ((val & 0x5555'5555'5555'5555ui64) << 1);
        val = ((val >> 2) & 0x3333'3333'3333'3333ui64) | ((val & 0x3333'3333'3333'3333ui64) << 2);
        val = ((val >> 4) & 0x0F0F'0F0F'0F0F'0F0Fui64) | ((val & 0x0F0F'0F0F'0F0F'0F0Fui64) << 4);
        val = ((val >> 8) & 0x00FF'00FF'00FF'00FFui64) | ((val & 0x00FF'00FF'00FF'00FFui64) << 8);
        val = ((val >> 16) & 0x0000'FFFF'0000'FFFFui64) | ((val & 0x0000'FFFF'0000'FFFFui64) << 16);
        val = (val >> 32) | (val << 32);
        return (val >> (64 - bits));
    }
}
