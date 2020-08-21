#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
template <class T>
using vec = std::vector<T>;

template <class T>
using mat = vec<vec<T>>;

template <class T>
using uset = std::unordered_set<T>;

template <class T1, class T2>
using mmap = std::unordered_multimap<T1, T2>;

template <class T>
using heap = std::priority_queue<T, vec<T>, std::greater<T>>;

class Solution {
public:
	struct State {
		int key, edge_count, max_weight, vertex;

		State() = default;

		State(int key, int edge_count, int max_weight, int vertex)
			: key(key), edge_count(edge_count), max_weight(max_weight),
			  vertex(vertex) {}

		bool operator>(const State& rhs) const { return this->key > rhs.key; }
	};

	auto solve(mat<int>& edges) {
		mmap<int, std::pair<int, int>> adj_map;
		auto target = std::numeric_limits<int>::min();

		for (auto& edge : edges) {
			auto u = edge[0];
			auto v = edge[1];
			auto w = edge[2];

			adj_map.emplace(u, std::make_pair(v, w));
			adj_map.emplace(v, std::make_pair(u, w));

			target = std::max({target, u, v});
		}

		heap<State> pq;
		pq.emplace(0, 0, 0, 0);

		uset<int> visited;

		while (!pq.empty()) {
			auto curr_state = pq.top();
			pq.pop();

			if (curr_state.vertex == target) {
				return curr_state.key;
			}

			auto it = visited.find(curr_state.vertex);
			if (it != visited.end()) {
				continue;
			}

			visited.emplace_hint(it, curr_state.vertex);

			auto edge_range = adj_map.equal_range(curr_state.vertex);
			for (auto it = edge_range.first; it != edge_range.second; ++it) {
				auto edge = it->second;

				State next_state;

				next_state.edge_count = curr_state.edge_count + 1;
				next_state.max_weight =
						std::max(curr_state.max_weight, edge.second);
				next_state.key = next_state.edge_count * next_state.max_weight;
				next_state.vertex = edge.first;

				pq.emplace(next_state);
			}
		}

		return -1;
	}
};
} // namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	int n;
	std::cin >> n;

	mat<int> edges;
	edges.reserve(n);

	int u, v, w;
	while (n--) {
		std::cin >> u >> v >> w;
		edges.emplace_back(std::vector<int>{u, v, w});
	}

	Solution solution;
	std::cout << solution.solve(edges) << ENDL;

	return 0;
}