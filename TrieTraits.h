#pragma once
#include <cstdint> // uint64_t, size_t
#include <limits> // std::numeric_limits<char>::max
#include <string>
#include <stack>
#include "StaticStack.h"

#include <cassert>
#define vassert assert

// Each type to be used in a Trie should have a specialization of this class, similar to const char* and uint64_t
template <typename T>
class TrieTraits;

// Example specialization for constructing string tries
template <>
class TrieTraits<const char*> {
public:
	// # of pointers at each trie level
	static constexpr size_t numPointers = std::numeric_limits<char>::max() + 1;
	// "Consumes" a char from the value, advances the value & returns the index of said char in the pointer array
	static size_t advance(const char*& str) { return *(str++); }
	// Whether a value has been completely consumed
	static bool consumed(const char* str) { return (*str == '\0'); }
	// Whether consuming a value is noexcept, to aid in Trie noexcept specification
	static constexpr bool noexceptConsume = true;

	// A single, temporary value of this type is maintained & used in a stack-like manner during trie in-order traversal.
	// Will be zero-initialized before usage.
	using U = std::string;
	// Obtains an actual value from the temporary, corresponding to current traversal progress
	static const char* fromTemporary(const U& str) { return str.c_str(); }
	// When descending a trie level, "appends" to the temporary value (given the pointer index & node depth)
	static void push(U& str, const size_t idx, const size_t depth) {
		vassert(str.size() == depth);
		str.push_back(char(idx));
	}
	// When ascending a trie level, "removes" from the temporary value (again given the popped node depth)
	static void pop(U& str, const size_t depth) {
		str.pop_back();
		vassert(str.size() == depth);
	}
	// A container with the ability to fit as many values as the # of nodes in the longest root-leaf path in trie.
	// For strings, this is unlimited (see other specializations for counter-example)
	template <typename T1>
	using Stack = std::stack<T1>;
	// Whether stack operation is noexcept, to aid in Trie noexcept specification
	static constexpr bool noexceptStack = false;
};

// Integers are represented as strings of numBits-sized chunks, starting from the lowest-significance bits.
// This means in-order trie traversal will return value in a weird, inverse-lexicological ordering.
// On the other side, limited string length removes the need for dynamically allocating trie paths during traversal/deletion.
template <> // to-do: find a way to specialize for all unsigned integers
class TrieTraits<uint64_t> {
	static constexpr size_t numBits = 4; // # of bits, consumed at each trie level. Change to 1 for a radix trie.
	static constexpr size_t maxDepth = (64 / numBits) + (64 % numBits != 0); // Maximim node depth in tree. Root depth is 0.
public:
	static constexpr size_t numPointers = (1ULL << numBits);
	static bool consumed(const uint64_t& value) { return (value == 0); }
	static size_t advance(uint64_t& value) { size_t res = value % numPointers; value >>= numBits; return res; }
	static constexpr bool noexceptConsume = true;

	using U = uint64_t;
	static uint64_t fromTemporary(const U& value) { return value; }
	static void push(U& value, const size_t idx, const size_t depth) { value += (idx << (numBits * depth)); }
	static void pop(U& value, const size_t depth) { value &= ~((numPointers - 1) << (numBits * depth)); }

	template <typename T1>
	using Stack = StaticStack<T1, maxDepth + 1>; // Trie path length limit (+1 for root node)
	static constexpr bool noexceptStack = true;
};

#undef vassert
