#pragma once
#include <cppcoro/generator.hpp> // cppcoro::generator
#include "TrieTraits.h"

template <template <typename> class Trie, typename T>
class TrieTraversal {
	using Traits = TrieTraits<T>;
public:
	// Traverses down the trie, checking whether a value is present
	bool contains(T value) noexcept(Traits::noexceptConsume) {
		const auto& t = reinterpret_cast<const Trie<T>&>(*this);
		typename Trie<T>::pointer curr = t.getRootPtr();
		while (!Traits::consumed(value)) {
			const size_t idx = Traits::advance(value);
			if (!t.hasChild(curr, idx)) {
				return false;
			}
			curr = t.getChild(curr, idx);
		}
		return t.hasValue(curr);
	}
	// Traverses the trie depth-first & returns each value found, keeping the path to the current node as the coroutine state.
	cppcoro::generator<T> values() noexcept(Traits::noexceptStack) {
		const auto& t = reinterpret_cast<const Trie<T>&>(*this);
		struct VisitState {
			typename Trie<T>::pointer root; // (sub)tree root
			size_t nextIdx; // Index of next subtree of root to be visited
		};
		// Stack of not-completely-visited subtrees (each with current visit progress)
		using Stack = Traits::template Stack<VisitState>;
		Stack path = {};
		path.push({ t.getRootPtr(), 0 });
		// The value, "built" from the path from the root to the current node.
		typename Traits::U temp = {};
		while (true) {
			auto& [curr, nextIdx] = path.top();
			// Return the value only on the first node visit
			if (nextIdx == 0 && t.hasValue(curr)) {
				co_yield Traits::fromTemporary(temp);
			}
			// Jump to next non-empty subtree
			while (nextIdx < Traits::numPointers && !t.hasChild(curr, nextIdx)) {
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
				path.push({ t.getChild(curr, nextIdx), 0 });
				++nextIdx; // For when we return to the current depth back again - continue from the next subtree
			}
		}
	}
};
