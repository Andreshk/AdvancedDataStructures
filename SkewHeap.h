#pragma once
#include <vector>
#include <functional> // std::less
#include <utility> // std::exchange

template <typename T>
struct SNode {
    T value;
    int left, right; // Children node indices inside the same buffer *this resides in.
    template <class... Args>
    SNode(Args&&... args) : value{ std::forward<Args>(args)... }, left{ -1 }, right{ -1 } {}
};

// Skew heap, supporting only inserting a value or replacing the min with a bigger (!) one.
template <typename T, typename Compare = std::less<T>>
class SkewHeap {
    std::vector<SNode<T>> allNodes;
    int rootIdx;
    [[no_unique_address]] Compare cmp;

    // O(lgn) amortized complexity, actually log_phi_n = 1.44*lgn approx.
    int skewMerge(const int left, const int right) {
        if (left == -1) {
            return right;
        } else if (right == -1) {
            return left;
        } else {
            auto& lhs = allNodes[left];
            auto& rhs = allNodes[right];
            if (cmp(lhs.value, rhs.value)) {
                const int res = skewMerge(lhs.right, right);
                lhs.right = lhs.left;
                lhs.left = res;
                return left;
            } else {
                const int res = skewMerge(left, rhs.right);
                rhs.right = rhs.left;
                rhs.left = res;
                return right;
            }
        }
    }
public:
    // Operates on a preallocated buffer (although resizing it it possible)
    SkewHeap(std::vector<SNode<T>>&& buff, const Compare& cmp = Compare{})
        : allNodes(std::move(buff)), rootIdx{ -1 }, cmp{ cmp } {}
    SkewHeap(const SkewHeap&) = delete;
    SkewHeap& operator=(const SkewHeap&) = delete;
    SkewHeap(SkewHeap&&) = delete;
    SkewHeap& operator=(SkewHeap&&) = delete;

    // Inserting a value - creates a singleton heap and merges with the current heap
    template <class... Args>
    void emplace(Args&&... args) {
        allNodes.emplace_back(std::forward<Args>(args)...);
        const int newNode = int(allNodes.size()) - 1;
        rootIdx = (rootIdx == -1 ? newNode : skewMerge(rootIdx, newNode));
    }

    // Attempts replacing the smallest element with one that's larger
    // than it & returns true on success. Note: no reallocations.
    bool tryReplaceTop(const T& newTop) {
        auto& root = allNodes[rootIdx];
        if (!cmp(newTop, root.value)) {
            root.value = newTop;
            // note: try bubbling down instead
            const int rest = skewMerge(std::exchange(root.left, -1), std::exchange(root.right, -1));
            rootIdx = skewMerge(rootIdx, rest);
            return true;
        } else {
            return false;
        }
    }

    // Returns ownership of the nodes vector & resets the heap to an empty one.
    [[nodiscard]] std::vector<SNode<T>> dump() {
        rootIdx = -1;
        return std::exchange(allNodes, {});
    }
};
