#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
template <class T>
using vec = std::vector<T>;

template <class T>
using mat = vec<vec<T>>;

class Solution {
public:
	bool solve(mat<int>& matrix, int target) {
		auto n = static_cast<int>(matrix.size());
		auto m = static_cast<int>(matrix[0].size());

		auto row = 0;
		auto col = m - 1;

		while (row < n && col >= 0) {
			auto curr = matrix[row][col];

			if (curr > target) {
				--col;
			} else if (curr < target) {
				++row;
			} else {
				return true;
			}
		}

		return false;
	}
};
} // namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	int n, m;
	std::cin >> n >> m;

	mat<int> matrix(n, vec<int>(m));
	for (auto& row : matrix) {
		for (auto& val : row) {
			std::cin >> val;
		}
	}

	int target;
	std::cin >> target;

	Solution solution;
	std::cout << solution.solve(matrix, target) << ENDL;

	return 0;
}