#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Solution {
public:
	auto solve(std::vector<int>& nums, int target) {
		auto res = 0;

		std::sort(nums.begin(), nums.end());

		auto n = static_cast<int>(nums.size());
		for (auto i = 0; i < n; ++i) {
			auto k = n - 1;

			for (auto j = i + 1; j < n; ++j) {
				while (k > j && nums[i] + nums[j] + nums[k] >= target) {
					--k;
				}

				if (k == j) {
					break;
				}

				res += k - j;
			}
		}

		return res;
	}
};
}// namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	int n;
	std::cin >> n;

	std::vector<int> nums;
	nums.reserve(n);

	int foo;
	while (n--) {
		std::cin >> foo;
		nums.emplace_back(foo);
	}

	int target;
	std::cin >> target;

	Solution solution;
	std::cout << solution.solve(nums, target) << ENDL;

	return 0;
}