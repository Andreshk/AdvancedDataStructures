#pragma once
#include <vector>
#include <ranges>
#include <algorithm> // std::ranges::{all_of,none_of}
#include <functional> // std::less
#include "vassert.h"
#include "TaggedArray.h"
#include "IntTags.h"

namespace {
const Int<ValueIdx> invalidIdx{ -1 };
struct Node {
    Int<ValueIdx> left = invalidIdx, right = invalidIdx;
};

// Instead of having a recursive function for 0-1RMQ construction,
// we simulate the call stack with a local one, containing all the info we need:
//   idx - index of the node
//   state - the next subtree to be visited
// Depending on the state we choose either to visit the left subtree next,
// visit the right one, or just backtrack up when there's nothing left to do.
enum class State { Left, Right, Done };
struct RMQFrame { Int<ValueIdx> idx; State state; };
} // anonymous namespace

struct RMQ01Info {
    DynamicArray<Int<Depth>, Int<RMQIdx>> depths;
    DynamicArray<Int<ValueIdx>, Int<RMQIdx>> indices;
    DynamicArray<Int<RMQIdx>, Int<ValueIdx>> occurences;
    RMQ01Info() = default;
    explicit RMQ01Info(const int n)
        : depths(Int<RMQIdx>(2 * n - 1))
        , indices(Int<RMQIdx>(2 * n - 1))
        , occurences(Int<ValueIdx>(n))
    {
        for (auto& x : occurences) { x = Int<RMQIdx>(-1); }
    }
};

// Min-cartesian tree, constructed for a sequence of values.
template <typename T, typename Comp = std::less<>>
class CartesianTree {
    // Nodes are only added and never removed, so we keep them all in contiguous
    // memory. The nodes themselves store indices to this array, instead of pointers
    // to other nodes (with invalidIdx corresponding to the null pointer).
    // Invariant: the node at index i correspongs to the i-th added value.
    DynamicArray<Node, Int<ValueIdx>> nodes;
    // Index of the tree's root node
    Int<ValueIdx> rootIdx;
    // Comparison functor
    Comp comp;
public:
    // Constructs a cartesian tree from a range of value &
    // optionally adds a number of sentinel values afterwards.
    template <std::ranges::input_range Range>
        requires std::is_same_v<T, std::ranges::range_value_t<Range>>
    explicit CartesianTree(const Range& values, const int numSentinels = 0, const Comp& comp = Comp{})
        : nodes(Int<ValueIdx>(int(values.size()) + numSentinels))
        , rootIdx{ 0 }
        , comp{ comp }
    {
        // We maintain the "right spine" of the tree, i.e. the nodes
        // visited by starting from the root and only traveling right.
        // This vector contains the indices of these nodes in the vector
        // and their corresponding values.
        struct Pair { T val; Int<ValueIdx> idx; };
        const auto from = std::ranges::cbegin(values);
        const auto to = std::ranges::cend(values);
        std::vector<Pair> rightSpine{ { *from, rootIdx } };
        // Index of the next value to be added (can be inferred by it)
        Int<ValueIdx> progress{ 1 };
        for (auto it = from + 1; it != to; ++it) {
            // Note that (obviously) every node is pushed "to the right",
            // i.e. all other nodes are to the left of it
            // - either "down left" or "up left" in the tree.
            const T& x = *it;
            if (comp(rightSpine.back().val, x)) {
                // Add another node to the bottom of the right spine, i.e. right child
                nodes[rightSpine.back().idx].right = progress;
                rightSpine.emplace_back(x, progress);
                ++progress;
            } else {
                // Find where to split the spine in order to insert the new node
                // Every node is initially inserted into the right spine, but once
                // removed, it will not be inserted back. This is why this loop
                // will take O(n) time total, resulting in the complexity of the algorithm.
                while (!rightSpine.empty() && !comp(rightSpine.back().val, x)) {
                    rightSpine.pop_back();
                }
                // If there are no nodes left in the spine, then the new node
                // should be the root (it is currently the smallest value in the tree)
                if (rightSpine.empty()) {
                    nodes[progress].left = rootIdx;
                    rootIdx = progress;
                    rightSpine.emplace_back(x, rootIdx);
                    ++progress;
                } else {
                    // Otherwise, we insert the new node at the split point of the spine
                    // and it becomes the "rightmost" node, inheriting the bottom part of
                    // the old spine as its left subtree.
                    const Int<ValueIdx> rightmostIdx = rightSpine.back().idx;
                    // These correspond to the "pointer" redirections for the nodes
                    nodes[progress].left = nodes[rightmostIdx].right;
                    nodes[rightmostIdx].right = progress;
                    // Should be noted that the newly inserted node always ends up
                    // as the rightmost node, i.e. the last node in the right spine.
                    rightSpine.emplace_back(x, progress);
                    ++progress;
                }
            }
        }
        // Simulate adding a number of sentinel values that compare larger
        // than all the given values, by forcing new nodes to the bottom of the right spine.
        Int<ValueIdx> lastIdx = rightSpine.back().idx;
        for (int i = 0; i < numSentinels; ++i) {
            nodes[lastIdx].right = progress;
            lastIdx = progress;
            ++progress;
        }
        vassert(progress == nodes.size());
    }

    RMQ01Info to01RMQ() const {
        if (nodes.empty()) {
            return {};
        }
        RMQ01Info res{ int(nodes.size()) };
        // The next index in the results arrays to be filled
        Int<RMQIdx> progress{ 0 };
        std::vector<RMQFrame> stack;
        // In the beginning, there was the root...
        stack.emplace_back(Int<ValueIdx>(rootIdx), State::Left);
        while (!stack.empty()) {
            auto& [idx,state] = stack.back();
            // We can infer the node's depth by the current stack size.
            const Int<Depth> depth{ int(stack.size() - 1) };
            // Each iteration corresponds to one node visit,
            // so we add its depth (and others) to the result no matter the state.
            res.depths[progress] = depth;
            res.indices[progress] = idx;
            res.occurences[idx] = progress;
            ++progress;
            const Node& nd = nodes[idx];
            // If the left subtree is due for visiting, but is empty,
            // change the state to the next (as if it's already visited)
            if (state == State::Left && nd.left == invalidIdx) {
                state = State::Right;
            }
            // Same for the right subtree - if it's empty, we are done with this node.
            if (state == State::Right && nd.right == invalidIdx) {
                state = State::Done;
            }
            // The above two checks also "recognize" the leaves in the tree.
            // Now either backtrack up, or select the next tree for visiting.
            // Update state before push_back, or the reference may be invalidated on reallocation (!)
            if (state == State::Left) {
                state = State::Right;
                stack.emplace_back(nd.left, State::Left);
            } else if (state == State::Right) { 
                // Mark for popping, but only on the way back from the right subtree
                state = State::Done;
                stack.emplace_back(nd.right, State::Left);
            } else {
                stack.pop_back();
            }
        }
        // Postconditions:
        if constexpr (vassertEnabled) {
            // The vector should be completely filled by now.
            vassert(progress == Int<RMQIdx>(res.depths.size()));
            // Each values should have at least one occurence noted
            vassert(std::ranges::none_of(res.occurences, [](const Int<RMQIdx> x) { return (x == -1); }));
            // Each value's occurence is the last (not actually required for correctness).
            vassert(std::ranges::all_of(std::views::iota(Int<RMQIdx>(0), res.indices.size()),
                [&](const Int<RMQIdx> pos) {
                    const Int<ValueIdx> valueIdx = res.indices[pos];
                    return (pos <= res.occurences[valueIdx]);
                }));
        }
        return res;
    }

    // External access (for debugging purposes)
    friend struct Printer;
};
// Deduction guide
template <std::ranges::input_range Range>
CartesianTree(const Range&) -> CartesianTree<std::ranges::range_value_t<Range>>;
template <std::ranges::input_range Range, typename Comp>
CartesianTree(const Range&, const int, const Comp&) -> CartesianTree<std::ranges::range_value_t<Range>, Comp>;
