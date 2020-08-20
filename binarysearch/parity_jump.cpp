#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Solution {
public:
#define BFS(flag)                                                             \
	{                                                                         \
		std::fill(visited.begin(), visited.end(), false);                     \
		std::fill(dist.begin(), dist.end(), -1);                              \
		for (auto i = 0; i < n; ++i) {                                        \
			if (nums[i] % 2 == flag) {                                        \
				q.emplace(i);                                                 \
				visited[i] = true;                                            \
				dist[i] = 0;                                                  \
			}                                                                 \
		}                                                                     \
		while (!q.empty()) {                                                  \
			auto u = q.front();                                               \
			q.pop();                                                          \
			for (auto& v : g[u]) {                                            \
				if (visited[v]) {                                             \
					continue;                                                 \
				}                                                             \
				q.emplace(v);                                                 \
				visited[v] = true;                                            \
				dist[v] = dist[u] + 1;                                        \
			}                                                                 \
		}                                                                     \
		for (auto i = 0; i < n; ++i) {                                        \
			if (nums[i] % 2 != flag) {                                        \
				best[i] = dist[i];                                            \
			}                                                                 \
		}                                                                     \
	}

	template <class T>
	using vec = std::vector<T>;

	auto solve(std::vector<int>& nums) {
		auto n = static_cast<int>(nums.size());
		vec<vec<int>> g(n, vec<int>());

		for (auto i = 0; i < n; ++i) {
			for (auto& neighbor : {i - nums[i], i + nums[i]}) {
				if (neighbor < 0 || neighbor >= n) {
					continue;
				}

				g[neighbor].emplace_back(i);
			}
		}

		std::queue<int> q;

		vec<int> dist(n);
		vec<int> best(n, -1);

		vec<bool> visited(n);

		BFS(false);
		BFS(true);

		return best;
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