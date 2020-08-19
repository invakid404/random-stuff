#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Solution {
public:
	std::vector<int> parent, rank;

	auto make_set(int v) {
		this->parent[v] = v;
		this->rank[v] = 0;
	}

	auto find_set(int v) {
		if (v == this->parent[v]) {
			return v;
		}

		return this->parent[v] = this->find_set(this->parent[v]);
	}

	auto union_sets(int a, int b) {
		a = this->find_set(a);
		b = this->find_set(b);

		if (a != b) {
			if (this->rank[a] < this->rank[b]) {
				std::swap(a, b);
			}

			this->parent[b] = a;

			if (this->rank[a] == this->rank[b]) {
				++this->rank[a];
			}
		}
	}

	auto solve(std::vector<std::vector<int>>& edges, int a, int b) {
		auto n = static_cast<int>(edges.size());

		parent.resize(n);
		rank.resize(n);

		for (auto i = 0; i < n; i++) {
			this->make_set(i);
		}

		std::sort(edges.begin(), edges.end(),
				  [&](const auto& a, const auto& b) {
					  return a[2] < b[2];
				  });

		for (auto& edge : edges) {
			if (this->find_set(edge[0]) != this->find_set(edge[1])) {
				if (a == edge[0] && b == edge[1]) {
					return true;
				}

				this->union_sets(edge[0], edge[1]);
			}
		}

		return false;
	}
};
}// namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	int n;
	std::cin >> n;

	std::vector<std::vector<int>> edges;

	int u, v, w;
	while (n--) {
		std::cin >> u >> v >> w;
		edges.emplace_back(std::vector<int>{u, v, w});
	}

	int a, b;
	std::cin >> a >> b;

	Solution solution;
	std::cout << solution.solve(edges, a, b) << ENDL;

	return 0;
}