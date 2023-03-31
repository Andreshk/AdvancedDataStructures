#pragma once
#include <cppcoro/generator.hpp> // preferrably from github/andreasbuhr
#include "TrieTraits.h"

template <typename T>
class TrieTraversal {
	using Traits = TrieTraits<T>;
public:
	// Traverses down the trie, checking whether a value is present
	bool contains(this const auto& self, T value) noexcept(Traits::noexceptConsume) {
		auto curr = self.getRootPtr();
		while (!Traits::consumed(value)) {
			const size_t idx = Traits::advance(value);
			if (!self.hasChild(curr, idx)) {
				return false;
			}
			curr = self.getChild(curr, idx);
		}
		return self.hasValue(curr);
	}
	// Traverses the trie depth-first & returns each value found, keeping the path to the current node as the coroutine state.
	cppcoro::generator<T> values(this const auto& self) noexcept(Traits::noexceptStack) {
		struct VisitState {
			decltype(self.getRootPtr()) root; // (sub)tree root
			size_t nextIdx; // Index of next subtree of root to be visited
		};
		// Stack of not-completely-visited subtrees (each with current visit progress)
		using Stack = Traits::template Stack<VisitState>;
		Stack path = {};
		path.push({ self.getRootPtr(), 0 });
		// The value, "built" from the path from the root to the current node.
		typename Traits::U temp = {};
		while (true) {
			auto& [curr, nextIdx] = path.top();
			// Return the value only on the first node visit
			if (nextIdx == 0 && self.hasValue(curr)) {
				co_yield Traits::fromTemporary(temp);
			}
			// Jump to next non-empty subtree
			while (nextIdx < Traits::numPointers && !self.hasChild(curr, nextIdx)) {
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
				path.push({ self.getChild(curr, nextIdx), 0 });
				++nextIdx; // For when we return to the current depth back again - continue from the next subtree
			}
		}
	}
};
