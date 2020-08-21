#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
template <class T>
using vec = std::vector<T>;

template <class T>
using mat = vec<vec<T>>;

template <class T>
using heap = std::priority_queue<T, vec<T>, std::greater<T>>;

class Solution {
public:
	struct State {
		vec<int>::iterator curr_it, end_it;

		State(vec<int>::iterator curr_it, vec<int>::iterator end_it)
			: curr_it(curr_it), end_it(end_it) {}

		bool operator>(const State& rhs) const {
			auto this_val = *(this->curr_it);
			auto other_val = *(rhs.curr_it);

			return this_val > other_val;
		}
	};

	auto solve(mat<int>& lists) {
		vec<int> res;
		heap<State> pq;

		auto total_n = 0;

		for (auto& list : lists) {
			auto curr_n = static_cast<int>(list.size());
			if (curr_n > 0) {
				pq.emplace(list.begin(), list.end());
				total_n += curr_n;
			}
		}

		res.reserve(total_n);

		while (!pq.empty()) {
			auto state = pq.top();
			pq.pop();

			res.emplace_back(*state.curr_it);

			auto next_it = std::next(state.curr_it);
			if (next_it != state.end_it) {
				pq.emplace(next_it, state.end_it);
			}
		}

		return res;
	}
};
} // namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	int n;
	std::cin >> n;

	mat<int> lists;
	lists.reserve(n);

	while (n--) {
		int m;
		std::cin >> m;

		std::vector<int> list;
		list.reserve(m);

		int foo;
		while (m--) {
			std::cin >> foo;
			list.emplace_back(foo);
		}

		lists.push_back(std::move(list));
	}

	Solution solution;
	auto res = solution.solve(lists);

	std::copy(res.begin(),
			  res.end(),
			  std::ostream_iterator<int>(std::cout, " "));
	std::cout << ENDL;

	return 0;
}