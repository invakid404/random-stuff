#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Solution {
public:
	auto solve(std::string& a, std::string& b) {
		auto m = static_cast<int>(a.size());
		auto n = static_cast<int>(b.size());

		if (abs(m - n) > 1)
			return false;

		int edits = 0;

		int i = 0, j = 0;
		while (i < m && j < n) {
			if (a[i] != b[j]) {
				edits++;

				if (edits >= 2) {
					return false;
				}

				if (m > n) { // If either string is longer => delete
					i++;
				} else if (m < n) {
					j++;
				} else { // Else => edit
					i++;
					j++;
				}
			} else {
				i++;
				j++;
			}
		}

		// Extra character?
		if (i < m || j < n) {
			edits++;
		}

		return edits <= 1;
	}
};
} // namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	std::string a, b;
	std::cin >> a >> b;

	Solution solution;
	std::cout << solution.solve(a, b) << ENDL;

	return 0;
}