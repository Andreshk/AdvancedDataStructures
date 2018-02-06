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

    // Since this is a static tree and we only add nodes during its construction,
    // we can store the nodes in continuous memory and keep indices to this
    // array instead of pointers to nodes. There are the size_t's the nodes contain,
    // instead of pointers to nodes. The constant invalidIdx corresponds to the null
    // pointer. Of course, we also shouldn't forget which node is the root node.
    std::vector<node> nodes;
    size_t rootIdx;
public:
    // O(n) construction
    cartesian_tree(const std::vector<int>& values) : nodes{}, rootIdx{ invalidIdx }
    {
        // Stupid, but necessary check
        if (values.empty())
            return;
        nodes.reserve(values.size());
        // We maintain the "right spine" of the tree, i.e. the nodes
        // visited by starting from the root and only traveling right.
        // This vector contains the indices of these nodes in the nodes vector.
        std::vector<size_t> rightSpine;
        // In the beginning there's only the root node,
        // which is also the only node in the right spine.
        nodes.emplace_back(values[0]);
        rootIdx = 0;
        rightSpine.push_back(rootIdx);
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
            // Find where to split the spine in order to insert the new node
            while (!rightSpine.empty() && nodes[rightSpine.back()].value >= curr)
                rightSpine.pop_back();
            // If there are no nodes left in the spine, then the new node
            // should be the root (it is currently the smallest value in the tree)
            if (rightSpine.empty())
            {
                nodes.emplace_back(curr);
                const size_t newIdx = nodes.size() - 1;
                nodes.back().left = rootIdx;
                rootIdx = newIdx;
                rightSpine.push_back(rootIdx);
                continue;
            }
            // Otherwise, we insert the new node at the split point of the spine
            // and it becomes the "rightmost" node, inheriting the bottom part of
            // the old spine as its left subtree.
            rightmostIdx = rightSpine.back();
            nodes.emplace_back(curr);
            const size_t newIdx = nodes.size() - 1;
            // These correspond to the "pointer" redirections for the nodes
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
        stack.emplace_back(frame{ rootIdx, 0 });
        // This print() function is not supposed to be fast,
        // so we can afford some maybe-not-inlined stateful lambdas for code clarity...
        auto printVal = [&](size_t idx) {
            if (idx == invalidIdx) 
                std::cout << "#";
            else 
                std::cout << nodes[idx].value;
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
            // Print the values in the children nodes
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
