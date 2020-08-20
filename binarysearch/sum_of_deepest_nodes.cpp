#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Tree {
public:
	int val;
	Tree* left;
	Tree* right;
};

#define SAFE_CALL(node, f)                                                    \
	{                                                                         \
		if (node != nullptr) {                                                \
			f(node);                                                          \
		}                                                                     \
	}

class Solution {
public:
	auto solve(Tree* root) {
		if (root == nullptr) {
			return 0;
		}

		int level_sum;

		auto q = std::queue<Tree*>();
		q.emplace(root);

		while (!q.empty()) {
			level_sum = 0;
			auto level_amt = q.size();

			while (level_amt--) {
				auto curr_node = q.front();
				q.pop();

				SAFE_CALL(curr_node->left, q.emplace);
				SAFE_CALL(curr_node->right, q.emplace);

				level_sum += curr_node->val;
			}
		}

		return level_sum;
	}
};
} // namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	// cba to deal with inputting a tree kekw

	return 0;
}