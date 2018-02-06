#pragma once
#include <iostream> // used for printing & debugging, may be removed
#include <vector>

// Min-cartesian tree
class cartesian_tree
{
    static const size_t invalidIdx = ~size_t{ 0 };
    struct node
    {
        int value;
        size_t left, right;
        node(int _val) : value{ _val }, left{ invalidIdx }, right{ invalidIdx } {}
    };

    // Since this is a static tree during construction we only add nodes,
    // we can store them in consecutive memory and keep indices in this
    // vector instead of pointers to nodes. This is why the nodes themselves
    // contain size_t instead of node*. invalidIdx corresponds to the null pointer.
    // Of course, we also shouldn't forget which node is the root node.
    std::vector<node> nodes;
    size_t root;
public:
    // O(n) construction
    cartesian_tree(const std::vector<int>& values)
    {
        // Stupid, but necessary check
        if (values.empty())
            return;
        nodes.reserve(values.size());
        // We maintain the "right spine" of the tree,
        // i.e. the nodes visited starting from the root and traveling right.
        // This vector contains the indices of these nodes in the nodes vector.
        std::vector<size_t> rightSpine;
        // Of course, in the beginning there's only the root node,
        // which is the only node in the right spine.
        nodes.emplace_back(values[0]);
        root = 0;
        rightSpine.push_back(root);
        // From then on, every node is pushed "to the right", meaning
        // all other nodes are to the left of it - either "down left" or "up left" in the tree.
        for (size_t i = 1; i < values.size(); ++i)
        {
            int curr = values[i];
            // We may need to actualize this idx, so it's not const
            size_t rightmostIdx = rightSpine.back();
            // Add another node to the right spine, i.e. right child
            if (curr > nodes[rightmostIdx].value)
            {
                nodes.emplace_back(curr);
                const size_t newIdx = nodes.size() - 1;
                nodes[rightmostIdx].right = newIdx;
                rightSpine.push_back(newIdx);
                continue;
            }
            // Find where to split the spine to insert the new node
            while (!rightSpine.empty() && nodes[rightSpine.back()].value >= curr)
                rightSpine.pop_back();
            // If there are no nodes left in the spine, this means the new node
            // should be the root (it is currently the smallest value in the tree)
            if (rightSpine.empty())
            {
                nodes.emplace_back(curr);
                const size_t newIdx = nodes.size() - 1;
                nodes.back().left = root;
                root = newIdx;
                rightSpine.push_back(root);
                continue;
            }
            // Otherwise, we insert the node node at the split point of the spine
            // and it becomes the "rightmost" node, inheriting the bottom part of
            // the old spine as its left subtree.
            rightmostIdx = rightSpine.back();
            nodes.emplace_back(curr);
            const size_t newIdx = nodes.size() - 1;
            // These indices correspond to the "pointer" redirections for the nodes
            nodes[newIdx].left = nodes[rightmostIdx].right;
            nodes[rightmostIdx].right = newIdx;
            // Should be noted that the newly inserted node always ends up
            // as the rightmost node, i.e. the last node in the right spine.
            rightSpine.push_back(newIdx);
        }
    }

    // Printing & debugging function, may be removed
    void print() const
    {
        if (nodes.empty())
        {
            std::cout << "Empty tree.\n";
            return;
        }
        // Instead of a recursive print() function, we keep a stack
        // of nodes waiting to be visited & printed. pad is used for pretty-printing.
        struct frame { size_t idx, pad; };
        std::vector<frame> stack;
        stack.emplace_back(frame{ root, 0 });
        // This print() function is not supposed to be fast,
        // so we can afford some maybe-not-inlined stateful lambdas for code clarity...
        auto printVal = [&](size_t idx) {
            if (idx == invalidIdx) std::cout << "#";
            else std::cout << nodes[idx].value;
        };
        auto stackPush = [&](size_t idx, size_t pad) {
            if (idx != invalidIdx) 
                stack.emplace_back(frame{ idx,pad }); 
        };
        // until there are no nodes left, print:
        while (!stack.empty())
        {
            // These should be bound by value, rather than reference (!)
            const auto [idx,pad] = stack.back();
            stack.pop_back();
            const node& nd = nodes[idx];
            // Print the padding, corresponding to the node depth
            for (size_t i = 0; i < pad; ++i) std::cout << ' ';
            std::cout << nd.value << "-> ";
            const size_t leftIdx = nd.left, rightIdx = nd.right;
            // In order to see more easily the children of a certain node
            printVal(leftIdx);
            std::cout << ", ";
            printVal(rightIdx);
            std::cout << "\n";
            // The right child is pushed deeper in the stack,
            // since we want to visit the left subtree first.
            stackPush(rightIdx, pad + 2);
            stackPush(leftIdx, pad + 2);
        }
    }
};
// iei
