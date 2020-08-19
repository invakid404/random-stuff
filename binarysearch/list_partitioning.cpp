#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Solution {
public:
	auto solve(std::vector<std::string>& strs) {
		auto n = static_cast<int>(strs.size());

		auto low = 0, mid = 1, high = n - 1;
		while (mid <= high) {
			switch (strs[mid][0]) {
			case 'r':
				std::swap(strs[low++], strs[mid++]);
				break;
			case 'g':
				++mid;
				break;
			default:
				std::swap(strs[mid], strs[high--]);
				break;
			}
		}

		return strs;
	}
};
}// namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	int n;
	std::cin >> n;

	std::vector<std::string> strs;
	strs.reserve(n);

	std::string foo;
	while (n--) {
		std::cin >> foo;
		strs.emplace_back(foo);
	}

	Solution solution;
	auto res = solution.solve(strs);

	std::copy(res.begin(), res.end(),
			  std::ostream_iterator<std::string>(std::cout, " "));
	std::cout << ENDL;

	return 0;
}