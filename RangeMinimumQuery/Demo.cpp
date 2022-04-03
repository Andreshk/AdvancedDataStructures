#include <fmt/core.h>
#include <fmt/ranges.h>
#include <span>
#include <vector>
#include <memory> // std::unique_ptr
#include <algorithm> // std::ranges::min_element

template <typename T>
struct Node {
	T value;
	std::unique_ptr<Node> left, right;
};

template <typename T>
std::unique_ptr<Node<T>> build(std::span<const T> values) {
	if (values.empty()) {
		return nullptr;
	} else { // We can add a values.size() == 1 case here if we want
		const auto it = std::ranges::min_element(values);
		const size_t pos = (it - values.begin());
		return std::make_unique<Node<T>>(
			*it,
			build(values.subspan(0, pos)),
			build(values.subspan(pos + 1))
		);
	}
}

// Note: when storing arrays of indices, no need to waste space for 64-bit integers
struct Node2 {
	int left, right;
};

template <typename T>
int fill(std::span<const T> values, std::span<Node2> nodes, const int firstIdx) {
	if (values.empty()) {
		return -1;
	} else { // We can add a values.size() == 1 case here if we want
		const auto it = std::ranges::min_element(values);
		const int pos = int(it - values.begin());
		nodes[pos].left = fill(values.subspan(0, pos), nodes.subspan(0, pos), firstIdx);
		nodes[pos].right = fill(values.subspan(pos + 1), nodes.subspan(pos + 1), firstIdx + pos + 1);
		return firstIdx + pos; // (!)
	}
}

int main1() {
	std::vector<int> values = { 3,4,2,1,5 };
	fmt::print(" Values: {}\nMinimum: ", values);

	std::unique_ptr<Node<int>> root = build(std::span<const int>{ values });
	fmt::print("{} ", root->value);

	std::vector<Node2> nodes(values.size());
	const int rootIdx = fill(std::span<const int>{ values }, nodes, 0);
	fmt::print("{}\n", values[rootIdx]);

	for (const auto& [l, r] : nodes) {
		fmt::print("({},{}) ", l, r);
	}
	fmt::print("\n");

	return 0;
}