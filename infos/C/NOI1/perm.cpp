#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = unsigned long long;
}

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	num n;
	std::cin >> n;

	std::vector<num> nums;
	nums.reserve(n);

	num curr_num[10];

	while (n--) {
		memset(curr_num, 0, sizeof(curr_num));

		std::string curr_str;
		std::cin >> curr_str;

		for (auto& curr_ch : curr_str) {
			++curr_num[curr_ch - '0'];
		}

		num res = 0;
		for (auto i = 9; i >= 0; --i) {
			auto curr_cnt = curr_num[i];
			while (curr_cnt--) {
				res *= 10;
				res += i;
			}
		}

		nums.emplace_back(res);
	}

	std::sort(nums.begin(), nums.end());

	num res = 0;
	for (auto it = nums.begin(); it != nums.end();) {
		auto last = std::upper_bound(it, nums.end(), *it);
		auto dist = std::distance(it, last);

		it = last;

		if (dist <= 1) {
			continue;
		}

		res += dist;
	}

	std::cout << res << ENDL;

	return 0;
}
