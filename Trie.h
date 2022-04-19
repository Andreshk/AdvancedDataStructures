#pragma once
#include "TrieTraits.h"
#include "TrieTraversal.h"

#include <concepts> // std::unsigned_integral
#include <cstdint> // uintptr_t, size_t
#include <array>
#include <bit> // std::bit_width, used for formatted output only

// Forward declaration
template <typename T>
	requires (TrieTraits<T>::numPointers <= 128)
class CompressedTrie;

// A trie node with N child nodes. Note that it doesn't depend on trie value type.
template <size_t N>
	requires (N >= 2 && N <= 256)
struct Node {
	// An array of pointers, in whose less-significant bit is encoded whether the pointed-to node contains a value
	std::array<uintptr_t, N> ptrs;
	Node() : ptrs{ 0 } {}
	// Helper pointer encoding/decoding functions
	static Node* decode(const uintptr_t ptr) { return (Node*)(ptr & ~uintptr_t(1)); }
	static uintptr_t encode(      Node* ptr, size_t hasValue) { return (uintptr_t(ptr) | hasValue); }
	static uintptr_t encode(const Node* ptr, size_t hasValue) { return (uintptr_t(ptr) | hasValue); }
	// Recursively free a subtree's memory
	void freeMem() noexcept {
		for (uintptr_t ptr : ptrs) {
			if (ptr) {
				Node::decode(ptr)->freeMem();
			}
		}
		delete this;
	}
};

// A generic trie class, supporting all types with complete TrieTraits specializations
template <typename T>
class Trie : public TrieTraversal<Trie, T> {
	using Traits = TrieTraits<T>;
	using Node = Node<Traits::numPointers>;

	Node root; // A trie always has a root node, so no need for a dynamic allocation
	size_t count; // # of values in tree.
	int maxBits_; // Max # of bits of all values in tree. Used for unsigned integers only.
	bool rootHasValue; // Whether root node contains a value
public:
	Trie() : rootHasValue{ 0 }, maxBits_{ 0 }, count{ 0 } {};
	Trie(const Trie&) = delete;
	Trie& operator=(const Trie&) = delete;
	Trie(Trie&& other) noexcept
		: root{ std::exchange(other.root, {}) }
		, count{ std::exchange(other.count, 0) }
		, maxBits_{ std::exchange(other.maxBits_, 0) }
		, rootHasValue{ std::exchange(other.rootHasValue, false) } {}
	Trie& operator=(Trie&& other) noexcept {
		root = std::exchange(other.root, {}); // No need for (this != &other) checks
		count = std::exchange(other.count, 0);
		maxBits_ = std::exchange(other.maxBits_, 0);
		rootHasValue = std::exchange(other.rootHasValue, false);
		return *this;
	}
	~Trie() { clear(); }
	// Convenience ctor
	Trie(std::initializer_list<T> il) : Trie{} {
		for (const T& value : il) { insert(value); }
	}
	// Attempts to insert a value, returns false iff already inserted.
	bool insert(T value) {
		if constexpr (std::unsigned_integral<T>) {
			maxBits_ = std::max(maxBits_, int(std::bit_width(value)));
		}
		// Traverse the existing path as far as possible
		uintptr_t toRoot = Node::encode(&root, rootHasValue); // Dummy variable to bootstrap traversal
		uintptr_t* curr = &toRoot;
		while (*curr && !Traits::consumed(value)) {
			Node* ptr = Node::decode(*curr);
			curr = &ptr->ptrs[Traits::advance(value)];
		}
		// If the entire path is traversed, a prefix has been inserted - mark the last node & nothing more to do.
		const bool inserted = !(*curr) || !((*curr) & 1);
		count += inserted;
		if (*curr) {
			(curr == &toRoot ? rootHasValue = true : *curr |= 1);
		} else while (!*curr) { // Otherwise, build the remainder of the path
			const bool consumed = Traits::consumed(value);
			Node* newNode = new Node;
			*curr = Node::encode(newNode, consumed);
			if (!consumed) {
				curr = &newNode->ptrs[Traits::advance(value)];
			}
		}
		return inserted;
	}
	// Attempts to remove a value, return true iff found.
	// Note: cannot update root.numBits in an efficient way, so it just doesn't touch it.
	bool remove(T value) noexcept(Traits::noexceptConsume && Traits::noexceptStack) {
		using Stack = Traits::template Stack<uintptr_t*>;
		Stack path = {};
		uintptr_t toRoot = Node::encode(&root, rootHasValue); // Dummy variable to bootstrap traversal
		uintptr_t* curr = &toRoot;
		// Traverse down the path, saving it in a stack
		while (*curr && !Traits::consumed(value)) {
			path.push(curr);
			Node* ptr = Node::decode(*curr);
			curr = &ptr->ptrs[Traits::advance(value)];
		}
		if (!*curr || !((*curr) & 1)) { // Value not found, nothing to do
			return false;
		}
		--count;
		(curr == &toRoot ? rootHasValue = false : *curr &= ~uintptr_t(1));
		path.push(curr); // One last push - this is the first node to be deleted
		// Small, helper function
		auto hasChildren = [](const uintptr_t root) {
			for (auto ptr : Node::decode(root)->ptrs) if (ptr) { return true; }
			return false;
		};
		// If the node found was a leaf, remove its entire branch - i.e. either up to a node with >1 children, with a value, or the root.
		while (path.size() > 1 && !((*path.top()) & 1) && !hasChildren(*path.top())) {
			delete Node::decode(*path.top());
			*path.top() = 0; // Tell the parent node it now has one child less
			path.pop();
		}
		return true;
	}
	// Maximum bit_width of all values ever inserted in the trie. Obviously applicable for unsigned integers only
	uint64_t maxBits() const noexcept requires std::unsigned_integral<T> { return maxBits_; }
	size_t size() const noexcept { return count; }
	bool empty() const noexcept { return (size() == 0); }

	// Clears all contents & resets to an empty trie
	void clear() noexcept {
		// Reminder not to delete &root - it's not dynamically allocated
		count = 0;
		maxBits_ = 0;
		rootHasValue = false;
		for (uintptr_t& ptr : root.ptrs) {
			if (ptr) {
				Node::decode(ptr)->freeMem();
				ptr = 0;
			}
		}
	}
private:
	// Typedefs & member functions, required by the TrieTraversal methods
	using pointer = uintptr_t;
	pointer getRootPtr() const { return Node::encode(&root, rootHasValue); }
	static bool hasValue(const pointer p) { return bool(p & 1); }
	static bool hasChild(const pointer p, size_t idx) { return bool(Node::decode(p)->ptrs[idx]); }
	static pointer getChild(const pointer p, size_t idx) { return Node::decode(p)->ptrs[idx]; }
	// (!)
	friend TrieTraversal<Trie, T>;
	friend CompressedTrie<T>;
};
