#pragma once
#include "Trie.h"
#include "TrieTraversal.h"
#include "StaticBitset.h" // from SnippySnippets repo
#include "vassert.h"
#include <type_traits> // std::conditional_t
#include <vector>
#include <queue>
#include <bit> // std::bit_ceil

// A compressed, static representation of a Trie. All original Trie nodes are
// indexed consecutively in a breadth-first manner, starting from 0 for the root.
// This guarantees that all children of a node will have consecutive indices,
// so that for each node we only need to keep the index of the first child;
// and the info which child pointers are non-null can be compressed in a bitset.
template <typename T>
    requires (TrieTraits<T>::numPointers <= 128) // No large enough bitset!
class CompressedTrie : public TrieTraversal<T> {
    using Traits = TrieTraits<T>;
    using Bitset = StaticBitset<std::bit_ceil(Traits::numPointers)>;
    using Index = typename Bitset::iterator::value_type;

    // For each node, a bitset indicating the position of non-null child pointers
    std::vector<Bitset> bitsets;
    // For each node, the index of its first child node + 1 bit indicating whether the node has a value
    std::vector<unsigned> firstChild;
    // # of values in tree
    size_t count;
    // Max # of bits of all values in tree. Used for unsigned integers only
    int maxBits_;
public:
    // Standard big 6
    CompressedTrie() : count{ 0 } {}
    CompressedTrie(const CompressedTrie&) = delete;
    CompressedTrie& operator=(const CompressedTrie&) = delete;
    CompressedTrie(CompressedTrie&& other) noexcept
        : bitsets{ std::move(other.bitsets) }
        , firstChild{ std::move(other.firstChild) }
        , maxBits_{ std::exchange(other.maxBits_, 0) }
        , count{ std::exchange(other.count, 0) } {}
    CompressedTrie& operator=(CompressedTrie&& other) noexcept {
        bitsets = std::move(other.bitsets);
        firstChild = std::move(other.firstChild);
        maxBits_ = std::exchange(other.maxBits_, 0);
        count = std::exchange(other.count, 0);
        return *this;
    }
    ~CompressedTrie() { clear(); }
    // Convenience ctor
    CompressedTrie(std::initializer_list<T> il) : CompressedTrie{ Trie<T>{il} } {}
    // (!)
    [[nodiscard]] CompressedTrie(const Trie<T>& t) : count{ t.count }, maxBits_{ t.maxBits_ } {
        using Node = typename Trie<T>::Node;
        std::queue<uintptr_t> q;
        q.push(Node::encode(&t.root, t.rootHasValue));
        unsigned nextIdx = 1;
        while (!q.empty()) {
            const uintptr_t node = q.front();
            const Node* ptr = Node::decode(node);
            q.pop();
            firstChild.push_back(nextIdx << 1 | unsigned(node & 1));
            Bitset curr{};
            for (size_t i = 0; i < Traits::numPointers; ++i) {
                if (ptr->ptrs[i]) {
                    curr.add(Index(i));
                    q.push(ptr->ptrs[i]);
                    ++nextIdx;
                }
            }
            bitsets.push_back(curr);
            vassert(bitsets.size() == firstChild.size());
        }
    }
    // Maximum bit_width of all values ever inserted in the trie. Obviously applicable for unsigned integers only
    uint64_t maxBits() const noexcept requires std::unsigned_integral<T> { return maxBits_; }
    size_t size() const noexcept { return count; }
    bool empty() const noexcept { return (size() == 0); }
    void clear() noexcept {
        std::exchange(bitsets, {}); // Force deallocations
        std::exchange(firstChild, {});
        count = 0;
    }
private:
    // Typedefs & member functions, required by the TrieTraversal methods
    using pointer = unsigned; // Nodes are uniquely identified by index
    pointer getRootPtr() const { return 0; }
    bool hasValue(const pointer p) const { return bool(firstChild[p] & 1); }
    bool hasChild(const pointer p, size_t idx) const { return bitsets[p].contains(Index(idx)); }
    pointer getChild(const pointer p, size_t idx) const {
        return (firstChild[p] >> 1) + int(bitsets[p].rank(Index(idx)));
    }
    // (!)
    friend TrieTraversal<T>;
};
