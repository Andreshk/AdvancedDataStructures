#pragma once
#include <fmt/core.h> // testing purposes only
#include <cppcoro/generator.hpp> // cppcoro::generator
#include "TrieTraits.h"

#include <concepts> // std::unsigned_integral, std::same_as
#include <cstdint> // size_t
#include <array>
#include <bit> // std::bit_width, used for formatted output only

// A trie node with N child nodes. Note that it doesn't depend on trie value type.
template <size_t N>
struct Node {
	static_assert(N >= 2 && N <= 256, "Invalid pointer array size!");

	size_t hasValue : 1; // Whether node contains a value
	size_t maxBits : 7; // Max # of bits of all values in tree. Used in the root node and for unsigned integers only. 7 bits, since 64 is a valid value
	size_t count : 56; // # of values in tree. Used in the root node only.
	std::array<Node*, N> ptrs;
	Node(bool hasValue = false) : hasValue{ hasValue }, maxBits{ 0 }, count{ 0 }, ptrs{ nullptr } {
		// Can't place these outside - class is considered incomplete there
		static_assert(sizeof(Node) == sizeof(ptrs) + sizeof(size_t));
		static_assert(alignof(Node) == alignof(size_t));
	}
	// Recursively free a subtree's memory
	void freeMem() noexcept {
		for (Node* ptr : ptrs) {
			if (ptr) {
				ptr->freeMem();
			}
		}
		delete this;
	}
};

// A generic trie class, supporting all types with complete TrieTraits specializations
template <typename T>
class Trie {
	using Traits = TrieTraits<T>;
	using Node = Node<Traits::numPointers>;

	// A trie always has a root node, so no need for a dynamic allocation
	Node root;
public:
	Trie() = default;
	Trie(const Trie&) = delete;
	Trie& operator=(const Trie&) = delete;
	Trie(Trie&& other) noexcept : root{ std::exchange(other.root, {}) } {}
	Trie& operator=(Trie&& other) noexcept {
		root = std::exchange(other.root, {}); // No need for (this != &other) checks
		return *this;
	}
	~Trie() { clear(); }
	// Attempts to insert a value, returns false iff already inserted.
	bool insert(T value) {
		if constexpr (std::unsigned_integral<T>) {
			root.maxBits = std::max(root.maxBits, std::bit_width(value));
		}
		// Traverse the existing path as far as possible
		Node* toRoot = &root; // Dummy variable to bootstrap traversal without an extra case for 0
		Node** ptr = &toRoot;
		while (*ptr && !Traits::consumed(value)) {
			ptr = &(*ptr)->ptrs[Traits::advance(value)];
		}
		// If the entire path is traversed, a prefix has been inserted - mark the last node & nothing more to do.
		const bool inserted = !(*ptr) || !(*ptr)->hasValue;
		root.count += inserted;
		if (*ptr) {
			(*ptr)->hasValue = true;
		} else while (!*ptr) { // Otherwise, build the remainder of the path
			const bool consumed = Traits::consumed(value);
			*ptr = new Node(consumed);
			if (!consumed) {
				ptr = &(*ptr)->ptrs[Traits::advance(value)];
			}
		}
		return inserted;
	}
	// Checks whether a value has been inserted
	bool contains(T value) const noexcept(Traits::noexceptConsume) {
		const Node* ptr = &root;
		while (ptr && !Traits::consumed(value)) {
			ptr = ptr->ptrs[Traits::advance(value)];
		}
		return (ptr && ptr->hasValue);
	}
	// Attempts to remove a value, return true iff found.
	// Note: cannot update root.numBits in an efficient way, so it just doesn't touch it.
	bool remove(T value) noexcept(Traits::noexceptConsume && Traits::noexceptStack) {
		using Stack = Traits::template Stack<Node**>;
		Stack path = {};
		Node* toRoot = &root; // Dummy variable to bootstrap traversal without an extra case for 0
		Node** curr = &toRoot;
		// Traverse down the path, saving it in a stack
		while (*curr && !Traits::consumed(value)) {
			path.push(curr);
			curr = &(*curr)->ptrs[Traits::advance(value)];
		}
		if (!*curr || !(*curr)->hasValue) { // Value not found, nothing to do
			return false;
		}
		(*curr)->hasValue = 0;
		--root.count;
		path.push(curr); // One last push - this is the first node to be deleted
		// Small, helper function
		auto hasChildren = [](const Node* root) {
			for (auto ptr : root->ptrs) if (ptr) { return true; }
			return false;
		};
		// If the node found was a leaf, remove its entire branch - i.e. either up to a node with >1 children, with a value, or the root.
		while (path.size() > 1 && !(*path.top())->hasValue && !hasChildren(*path.top())) {
			delete *path.top();
			*path.top() = nullptr; // Tell the parent node it now has one child less
			path.pop();
		}
		return true;
	}
	// Maximum bit_width of all values ever inserted in the trie. Obviously applicable for unsigned integers only
	uint64_t maxBits() const noexcept requires std::unsigned_integral<T> { return root.maxBits; }
	size_t size() const noexcept { return root.count; }
	bool empty() const noexcept { return (size() == 0); }

	// Traverses the trie depth-first & returns each value found, keeping the path to the current node as the coroutine state.
	cppcoro::generator<T> values() const noexcept(Traits::noexceptStack)  {
		struct VisitState {
			const Node* root; // (sub)tree root
			size_t nextIdx; // Index of next subtree of root to be visited
		};
		// Stack of not-completely-visited subtrees (each with current visit progress)
		using Stack = Traits::template Stack<VisitState>;
		Stack path = {};
		path.push({ &root, 0 });
		// The value, "built" from the path from the root to the current node.
		typename Traits::U temp = {};
		while (true) {
			auto& [curr, nextIdx] = path.top();
			// Return the value only on the first node visit
			if (nextIdx == 0 && curr->hasValue) {
				co_yield Traits::fromTemporary(temp);
			}
			// Jump to next non-empty subtree
			while (nextIdx < Traits::numPointers && !curr->ptrs[nextIdx]) {
				++nextIdx;
			}
			if (nextIdx == Traits::numPointers) {
				path.pop();
				if (path.empty()) {
					break; // We've popped the root, nothing more to do
				}
				Traits::pop(temp, path.size() - 1); // The stack top's depth
			} else { // Push the next subtree to the stack
				Traits::push(temp, nextIdx, path.size() - 1); // The stack top's depth
				path.push({ curr->ptrs[nextIdx], 0 });
				++nextIdx; // For when we return to the current depth back again - continue from the next subtree
			}
		}
	}

	// Clears all contents & resets to an empty trie
	void clear() noexcept {
		// Reminder not to delete &root - it's not dynamically allocated
		root.hasValue = root.count = root.maxBits = 0;
		for (Node*& ptr : root.ptrs) {
			if (ptr) {
				ptr->freeMem();
				ptr = nullptr;
			}
		}
	}
	
	// Simple demo for string insertion, search & removal
	static void test() requires std::same_as<T, const char*> {
		Trie<const char*> t;
		int numInserted = 0;
		for (const char* str : { "andi", "", "and", "andy", "baba" }) {
			if (t.insert(str)) {
				++numInserted;
			}
		}
		fmt::print("numInserted={}\ncount={}\n", numInserted, t.size());
		for (const char* str : t.values()) {
			fmt::print("\"{}\"\n", str);
		}
		for (const char* str : { "", "andi", "andrey", "ba", "c" }) {
			fmt::print("\"{}\" : {}\n", str, t.contains(str));
		}
		int numRemoved = 0;
		for (const char* str : { "andi", "", "and", "andy", "baba" }) {
			if (t.remove(str)) {
				++numRemoved;
			}
		}
		fmt::print("numRemoved={}\ncount={}\n", numRemoved, t.size());
		for (const char* str : t.values()) {
			fmt::print("\"{}\"\n", str);
		}
		fmt::print("\n");
	}
	// Simple demo for integer insertion, search & removal
	static void test() requires std::unsigned_integral<T> {
		Trie<T> t;
		int numInserted = 0;
		for (const T value : { 68, 20, 35, 32, 14, 0, 12, 20, 300, 0, 301, 420/*, -1, -2*/ }) {
			if (t.insert(value)) {
				++numInserted;
			}
		}
		fmt::print("numInserted={}\n", numInserted);
		fmt::print("count={} maxBits={}\n", t.size(), t.maxBits());
		for (const T value : t.values()) {
			// Print arg0 in binary, padding up to arg1 with zeroes; then print arg0 normally
			fmt::print("{0:0{1}b} ({0})\n", value, t.maxBits());
		}
		for (const T value : { 0, 5, 12, 20, 28, 68, 420 }) {
			fmt::print("{} : {}\n", value, t.contains(value));
		}
		int numRemoved = 0;
		for (const T value : { 68, 20, 35, 32, 14, 0, 12, 20, 300, 0, 301, 420/*, -1, -2*/ }) {
			if (t.remove(value)) {
				++numRemoved;
			}
		}
		fmt::print("numRemoved={}\n", numRemoved);
		fmt::print("count={} maxBits={}\n", t.size(), t.maxBits());
		for (const T value : t.values()) {
			// Print arg0 in binary, padding up to arg1 with zeroes; then print arg0 normally
			fmt::print("{0:0{1}b} ({0})\n", value, t.maxBits());
		}
		fmt::print("\n");
	}
};
