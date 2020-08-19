#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Solution {
public:
	template<class T>
	using vec = std::vector<T>;

	template<class T>
	using mat = vec<vec<T>>;

	auto solve(vec<int>& breakpoints) {
		auto n = static_cast<int>(breakpoints.size() - 1);
		if (n <= 0) {
			return 0;
		}

		vec<int> deltas;
		deltas.reserve(n);

		for (auto i = 0; i < n; ++i) {
			deltas.emplace_back(breakpoints[i + 1] - breakpoints[i]);
		}

		vec<int> prefix_sum;
		prefix_sum.reserve(n + 1);

		prefix_sum.emplace_back(0);
		for (auto i = 0; i < n; ++i) {
			prefix_sum.emplace_back(prefix_sum.back() + deltas[i]);
		}

		mat<int> dp(n, vec<int>(n));
		mat<int> idx(n, vec<int>(n));

		for (auto i = 0; i < n; ++i) {
			dp[i][i] = deltas[i];
			idx[i][i] = i;
		}

		for (auto d = 1; d < n; ++d) {
			for (auto i = 0; i < n - d; ++i) {
				auto j = i + d;
				auto v = std::numeric_limits<int>::max();
				auto p = -1;

				for (auto k = idx[i][j - 1]; k <= std::min(j - 1, idx[i + 1][j]); ++k) {
					auto val = dp[i][k] + dp[k + 1][j];
					if (val < v) {
						v = val;
						p = k;
					}
				}

				idx[i][j] = p;
				dp[i][j] = v + (prefix_sum[j + 1] - prefix_sum[i]);
			}
		}

		return dp[0][n - 1];
	}
};
}// namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	int n;
	std::cin >> n;

	std::vector<int> breakpoints;
	breakpoints.reserve(n);

	int foo;
	while (n--) {
		std::cin >> foo;
		breakpoints.emplace_back(foo);
	}

	Solution solution;
	std::cout << solution.solve(breakpoints) << ENDL;

	return 0;
}