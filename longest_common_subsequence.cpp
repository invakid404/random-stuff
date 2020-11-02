#include <bits/stdc++.h>

#define ENDL std::endl

namespace {
auto solve(std::string x, std::string y) {
	auto m = static_cast<int>(x.size());
	auto n = static_cast<int>(y.size());

	int dp[m + 1][n + 1];

	for (auto i = 0; i <= m; ++i) {
		for (auto j = 0; j <= n; ++j) {
			if (i == 0 || j == 0) {
				dp[i][j] = 0;
			} else if (x[i - 1] == y[j - 1]) {
				dp[i][j] = dp[i - 1][j - 1] + 1;
			} else {
				dp[i][j] = std::max(dp[i - 1][j], dp[i][j - 1]);
			}
		}
	}

	return dp[m][n];
}
} // namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	std::string a, b;
	std::cin >> a >> b;

	std::cout << solve(a, b) << ENDL;

	return 0;
}
