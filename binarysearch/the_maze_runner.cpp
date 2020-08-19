#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
template<class T>
using vec = std::vector<T>;

template<class T>
using mat = vec<vec<T>>;

class Solution {
public:
	using bfs_queue = std::queue<std::pair<int, int>>;

	const vec<std::pair<int, int>> deltas = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};

	auto go(mat<int>& matrix, bfs_queue& q, mat<bool>& vis_self, mat<int>& dist, mat<bool>& vis_other) {
		auto res_i = -1, res_j = -1;

		if (!q.empty()) {
			int curr_i, curr_j;
			std::tie(curr_i, curr_j) = q.front();
			q.pop();

			vis_self[curr_i][curr_j] = true;

			for (auto& delta : this->deltas) {
				auto next_i = curr_i + delta.first;
				auto next_j = curr_j + delta.second;

				if (next_i < 0 || next_i >= matrix.size() || next_j < 0 || next_j >= matrix[0].size()) {
					continue;
				}

				if (vis_self[next_i][next_j] || matrix[next_i][next_j]) {
					continue;
				}

				q.emplace(next_i, next_j);

				vis_self[next_i][next_j] = true;
				dist[next_i][next_j] = 1 + dist[curr_i][curr_j];

				if (vis_other[next_i][next_j]) {
					res_i = next_i;
					res_j = next_j;
				}
			}
		}

		return std::make_pair(res_i, res_j);
	}

	auto solve(mat<int>& matrix) {
		auto n = static_cast<int>(matrix.size());
		if (n <= 0) {
			return -1;
		}

		auto m = static_cast<int>(matrix[0].size());

		if (matrix[0][0] || matrix[n - 1][m - 1]) {
			return -1;
		}

		bfs_queue start_queue, target_queue;

		start_queue.emplace(0, 0);
		target_queue.emplace(n - 1, m - 1);

		mat<bool> start_visited(n, vec<bool>(m, false));
		mat<bool> target_visited(n, vec<bool>(m, false));

		start_visited[0][0] = target_visited[0][0] = true;

		mat<int> start_distance(n, vec<int>(m, 0));
		mat<int> target_distance(n, vec<int>(m, 0));

		while (!start_queue.empty() || !target_queue.empty()) {
			int res_i, res_j;
			std::tie(res_i, res_j) = go(matrix, start_queue, start_visited,
										start_distance, target_visited);
			if (res_i != -1 && res_j != -1) {
				return 1 + start_distance[res_i][res_j] + target_distance[res_i][res_j];
			}

			std::tie(res_i, res_j) = go(matrix, target_queue, target_visited,
										target_distance, start_visited);
			if (res_i != -1 && res_j != -1) {
				return 1 + start_distance[res_i][res_j] + target_distance[res_i][res_j];
			}
		}

		return -1;
	}
};
}// namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	int n, m;
	std::cin >> n >> m;

	mat<int> matrix(n, vec<int>(m));
	for (auto i = 0; i < n; ++i) {
		for (auto j = 0; j < m; ++j) {
			std::cin >> matrix[i][j];
		}
	}

	Solution solution;
	std::cout << solution.solve(matrix) << ENDL;

	return 0;
}