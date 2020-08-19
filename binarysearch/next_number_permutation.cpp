#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Solution {
public:
	auto num_to_vec(int num) {
		std::vector<int> digits;

		while (num) {
			auto d = std::div(num, 10);

			digits.emplace_back(d.rem);
			num = d.quot;
		}

		std::reverse(digits.begin(), digits.end());

		return digits;
	}

	auto vec_to_num(const std::vector<int>& digits) {
		auto res = 0;

		for (auto& digit : digits) {
			res *= 10;
			res += digit;
		}

		return res;
	}

	int solve(int num) {
		auto digits = num_to_vec(num);

		std::next_permutation(digits.begin(), digits.end());

		return vec_to_num(digits);
	}
};
} // namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	int n;
	std::cin >> n;

	Solution solution;
	std::cout << solution.solve(n) << ENDL;

	return 0;
}