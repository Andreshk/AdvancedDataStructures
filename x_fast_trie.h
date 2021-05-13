#pragma once
#include <bit> // std::bit_width
#include <array>
#include <vector>
#include <limits> // std::numeric_limits::max
#include <cstdint>  // uint{32,64}_t
#include <cstddef>  // size_t
#include <utility>  // std::exchange
#include <concepts> // std::unsigned_integral
#include <optional>
#include <unordered_map>
#include "vassert.h"

#ifdef XFAST_DEBUG_PRINT
#include <iostream> // printing & debugging purposes
#endif // XFAST_DEBUG_PRINT

// Forward declarations of minor helper functions
template <std::unsigned_integral Value>
int maxBitWidth(const std::vector<Value>&);
template <std::unsigned_integral Value>
Value reverseBits(Value, const int);

template <std::unsigned_integral Value = uint64_t>
class XFastTrie {
    // This is allowed to have an arbitrary underlying type and will not be
    // intermixed with Value in the code - they simply have different semantics.
    // Of course, this type limits the number of values contained.
    using Idx = int;
    using Pair = std::array<Idx, 2>;
    using HashMap = std::unordered_map<Value, Pair>;

    // Every Node contains the indices of the values, corresponding
    // to its left- and rightmost leaf in the temporary trie.
    static constexpr Idx invalidIdx = -1;
    static constexpr Idx maxSize = std::numeric_limits<Idx>::max() / 2;
    static bool isInvalid(const Idx idx) { return (idx == invalidIdx); }
    struct Node {
        Idx children[2];
        Node() : children{ invalidIdx,invalidIdx } {};
    };

    std::vector<Value>   values; // the values contained, in sorted order
    std::vector<HashMap> levels; // for every trie level, a hashmap to pairs of leaf indices
    int bits;                    // the number of bits, required to represent the biggest value

    // Insert a value in the temporary trie, represented by the nodes array
    void trieInsert(std::vector<Node>& nodes, const Value val) {
        // Reverse the "significant" bits for easier access
        Value rval = reverseBits(val, bits);
        int restBits = bits;
        Idx currIdx = 0;
        // Navigate common prefix in tree
        while (true) {
            Idx childIdx = nodes[currIdx].children[rval & 1];
            if (isInvalid(childIdx)) {
                break;
            }
            currIdx = childIdx;
            rval >>= 1;
            --restBits;
        }
        // Add the rest of the bits
        while (restBits) {
            nodes.emplace_back(); // an empty node
            Idx lastIdx = Idx(nodes.size()) - 1;
            nodes[currIdx].children[rval & 1] = lastIdx;
            currIdx = lastIdx;
            rval >>= 1;
            --restBits;
        }
    }
    
    // The crux of Willard's algorithm
    std::optional<Value> find(const Value x, const bool succ) const {
        const std::optional<Value> none{ std::nullopt };
        if (empty()) {
            return none;
        }
        // Do not search for values outside of the range
        if (x > values.back()) {
            return (succ ? none : values.back());
        } else if (x < values.front()) {
            return (succ ? values.front() : none);
        }
        // This also catches the corner cases of the last value's
        // successor and the first value's predeccessor.
        if (levels.back().contains(x)) {
            return x;
        }
        // Find the longest matching prefix of the searched value
        // by binary searching through the trie levels
        int l = 0, r = bits;
        while (r - l > 1) {
            const int m = (l + r) / 2;
            const int i = bits - m;
            if (levels[m].contains(x >> i)) {
                l = m;
            } else {
                r = m;
            }
        }
        // xSplit == longest matching prefix
        const Value xSplit = x >> (bits - l);
        // The contents of the "splitting" node, i.e. its value indices
        const Pair& arr = levels[l].at(xSplit);
        // Now do some more magic to save a couple of branches...
        static const int index[] = { -1,1 };
        // For starters, we check where this node has a left or a right
        // child node (we know there is exactly one) by another hashmap lookup
        if (levels[l + 1].contains(2 * xSplit + succ)) {
            return values[arr[!succ]];
        } else {
            return values[arr[succ] + index[succ]];
        }
    }
public:
    // Here be magic
    XFastTrie(const std::vector<Value>& values) : bits{ maxBitWidth(values) } {
        // Very, very unlikely...
        if (values.empty() || values.size() >= maxSize) {
            return;
        }
        const size_t n = values.size();
        // The final Node count is expected to be around 2n. The trie is
        // actually a temporary structure, needed only at initialization!
        std::vector<Node> nodes{ 1,Node{} }; // there's always a root Node
        nodes.reserve(2 * n);
        // First build the trie...
        for (auto val : values) {
            trieInsert(nodes, val);
        }
        // ...then save its levels in an array of hashmaps.
        levels.reserve(bits + 1);
        // Level by level, including the root and the leaves...
        // currLevel contains prefixes and currNodes the indices
        // of the respective nodes. Buffers will be used as buffers.
        std::vector<Value> currLevel{ 0 }, levelBuff;
        std::vector<Idx> currNodes{ 0 /*root idx*/ }, nodesBuff;
        levelBuff.reserve(n);
        nodesBuff.reserve(n);
        // Builds a new hashmap from the values in the current level & adds it to the array
        auto makeLevelHash = [this, &currLevel, &currNodes]() {
            vassert(currLevel.size() == currNodes.size());
            HashMap& hash = levels.emplace_back();
            for (size_t i = 0; i < currLevel.size(); ++i) {
                hash.emplace(currLevel[i], Pair{ currNodes[i], invalidIdx });
            }
        };
        for (int b = 0; b < bits; ++b) {
            // In the beginning, the hashmaps map a prefix value to its trie Node
            makeLevelHash();
            // Update to the next level
            for (size_t i = 0; i < currNodes.size(); ++i) {
                for (int pos = 0; pos < 2; ++pos) { // unroll this pls
                    const Idx childIdx = nodes[currNodes[i]].children[pos];
                    if (!isInvalid(childIdx)) {
                        levelBuff.push_back(2 * currLevel[i] + pos);
                        nodesBuff.push_back(childIdx);
                    }
                }
            }
            std::swap(currNodes, nodesBuff);
            std::swap(currLevel, levelBuff);
            // clear() almost always keeps the vector capacity,
            // so level updating should cause no reallocations.
            nodesBuff.clear();
            levelBuff.clear();
        }
        // Unfortunately, we also need the last level in a hashmap for quick lookups
        makeLevelHash();
        // Now all indices in currNodes point to the leaves, in sorted order
        this->values = std::move(currLevel); // don't need this anymore
        for (size_t i = 0; i < currNodes.size(); ++i) {
            Node& node = nodes[currNodes[i]];
            node.children[0] = Idx(i);
            node.children[1] = Idx(i);
        }
        // Traverse the tree backwards, updating the value indices
        // Until now, each Node contains the indices of its two child nodes
        vassert(levels.size() == bits + 1);
        for (int b = bits - 1; b >= 0; --b) {
            for (const auto& p : levels[b]) {
                Node& node = nodes[p.second[0]];
                // Child Node indices (will be equal when Node has an only child)
                const Idx left_idx = node.children[isInvalid(node.children[0])];
                const Idx right_idx = node.children[!isInvalid(node.children[1])];
                node.children[0] = nodes[left_idx].children[0];
                node.children[1] = nodes[right_idx].children[1];
            }
        }
        // Now every Node contains the indices of the values, corresponding to
        // its left- and rightmost leaves, but the hashmaps still contain Node indices.
        // So we replace each Node's index with its contents and discard the trie itself.
        for (HashMap& level : levels) {
            for (auto& p : level) {
                Pair& arr = p.second;
                const Node& node = nodes[arr[0]];
                arr[0] = node.children[0];
                arr[1] = node.children[1];
            }
        }
    }
    XFastTrie(XFastTrie&& other) noexcept
        : values{ std::move(other.values) }, levels{ std::move(other.levels) }, bits{ std::exchange(other.bits, 0) } {}
    XFastTrie& operator=(XFastTrie&& other) noexcept {
        if (this != &other) {
            this->~XFastTrie();
            new (this) XFastTrie(std::move(other)); // lol
        }
        return *this;
    }
    ~XFastTrie() { clear(); }

    // Finding the previous and next value are dual operations, after all.
    std::optional<Value> pred(Value x) const { return find(x, false); }
    std::optional<Value> succ(Value x) const { return find(x, true); }

    // Standard...
    size_t size() const noexcept { return values.size(); }
    bool empty() const noexcept { return values.empty(); }
    void clear() noexcept {
        std::exchange(values, {}); // Force deallocation
        std::exchange(levels, {});
        bits = 0;
    }

#ifdef XFAST_DEBUG_PRINT
    // Prints detailed trie information, incl. level hashmaps
    // and whether the leaves point to the correct values
    void print() const {
        std::cout << "Bits: " << bits << "\nValue count: " << size() << "\n";
        if (empty() || size() > 50) {
            return;
        }
        auto print_bits = [](const Value val, const int bits) {
            std::cout << val << '[';
            for (int i = bits - 1; i >= 0; --i) {
                std::cout << ((val >> i) & 1 ? '1' : '0');
            }
            std::cout << ']';
        };
        std::cout << "\nValues (sorted):\n";
        for (const Value x : values) {
            print_bits(x, bits);
            std::cout << "\n";
        }
        std::cout << "\nHashes:\n";
        for (int i = 0; i < int(levels.size()); ++i) {
            std::cout << "  Level " << i << "\n";
            for (const auto& [val, p] : levels[i]) {
                std::cout << "val: "; print_bits(val, i);
                std::cout << " {"; print_bits(values[p[0]], bits);
                std::cout << ",";  print_bits(values[p[1]], bits);
                std::cout << "}\n";
            }
        }
    }
#endif // XFAST_DEBUG_PRINT
};

// Utilities
// This begs for vectorization
template <std::unsigned_integral Value>
int maxBitWidth(const std::vector<Value>& values) {
    int result = 0;
    for (const Value x : values) {
        const int curr = int(std::bit_width(x));
        if (result < curr) {
            result = curr;
        }
    }
    return result;
}

// There are other ways to flip bits, hopefully this one isn't too slow
template <>
uint8_t reverseBits(uint8_t val, const int bits) {
    val = ((val >> 1) & 0x55ui8) | ((val & 0x55ui8) << 1);
    val = ((val >> 2) & 0x33ui8) | ((val & 0x33ui8) << 2);
    val = (val >> 4) | (val << 4);
    return (val >> (8 - bits));
}
template <>
uint16_t reverseBits(uint16_t val, const int bits) {
    val = ((val >> 1) & 0x5555ui16) | ((val & 0x5555ui16) << 1);
    val = ((val >> 2) & 0x3333ui16) | ((val & 0x3333ui16) << 2);
    val = ((val >> 4) & 0x0F0Fui16) | ((val & 0x0F0Fui16) << 4);
    val = (val >> 8) | (val << 8);
    return (val >> (16 - bits));
}
template <>
uint32_t reverseBits(uint32_t val, const int bits) {
    val = ((val >> 1) & 0x5555'5555ui32) | ((val & 0x5555'5555ui32) << 1);
    val = ((val >> 2) & 0x3333'3333ui32) | ((val & 0x3333'3333ui32) << 2);
    val = ((val >> 4) & 0x0F0F'0F0Fui32) | ((val & 0x0F0F'0F0Fui32) << 4);
    val = ((val >> 8) & 0x00FF'00FFui32) | ((val & 0x00FF'00FFui32) << 8);
    val = (val >> 16) | (val << 16);
    return (val >> (32 - bits));
}
template <>
uint64_t reverseBits(uint64_t val, const int bits) {
    val = ((val >> 1) & 0x5555'5555'5555'5555ui64) | ((val & 0x5555'5555'5555'5555ui64) << 1);
    val = ((val >> 2) & 0x3333'3333'3333'3333ui64) | ((val & 0x3333'3333'3333'3333ui64) << 2);
    val = ((val >> 4) & 0x0F0F'0F0F'0F0F'0F0Fui64) | ((val & 0x0F0F'0F0F'0F0F'0F0Fui64) << 4);
    val = ((val >> 8) & 0x00FF'00FF'00FF'00FFui64) | ((val & 0x00FF'00FF'00FF'00FFui64) << 8);
    val = ((val >> 16) & 0x0000'FFFF'0000'FFFFui64) | ((val & 0x0000'FFFF'0000'FFFFui64) << 16);
    val = (val >> 32) | (val << 32);
    return (val >> (64 - bits));
}
