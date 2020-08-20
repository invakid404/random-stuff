#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Solution {
public:
	auto solve(std::vector<int>& nums) {
		auto product = 1, zero_cnt = 0;

		for (auto& num : nums) {
			if (num == 0) {
				++zero_cnt;
			} else {
				product *= num;
			}
		}

		switch (zero_cnt) {
		case 0:
			for (auto& num : nums) {
				num = product / num;
			}

			break;
		case 1:
			for (auto& num : nums) {
				if (num == 0) {
					num = product;
				} else {
					num = 0;
				}
			}

			break;
		default:
			std::fill(nums.begin(), nums.end(), 0);
		}

		return nums;
	}
};
} // namespace

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

	Solution solution;
	auto res = solution.solve(nums);

	std::copy(res.begin(),
			  res.end(),
			  std::ostream_iterator<int>(std::cout, " "));

	std::cout << ENDL;

	return 0;
}