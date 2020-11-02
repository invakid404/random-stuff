#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
auto search(std::vector<int>& v, int c = 0, int idx = 0) -> int {
	auto res = static_cast<int>(__builtin_popcount(c));

	auto n = static_cast<int>(v.size());
	if (idx < n) {
		auto curr = v[idx];

		// If the current string doesn't overlap with the accumulated one
		if (!(c & curr)) {
			// Concatenate
			c ^= curr;

			// Recursively try the next character after concatenating
			res = std::max(res, search(v, c, idx + 1));

			// Backtrack
			c ^= curr;
		}

		// Recursively try the next character without concatenating
		res = std::max(res, search(v, c, idx + 1));
	}

	return res;
}

auto solve(std::vector<std::string>& words) {
	std::vector<int> v;
	v.reserve(words.size());

	// Convert words to integers
	// Useful for fast concatenation and intersection via bitwise operations
	for (auto& word : words) {
		auto curr = 0;
		auto ok = true;

		for (auto& c : word) {
			auto ord = c - 'a';
			auto curr_bit = 1 << ord;

			// If a word has a duplicate within itself,
			// it will never be part of the answer
			if (curr & curr_bit) {
				ok = false;

				break;
			}

			curr |= curr_bit;
		}

		if (ok) {
			v.emplace_back(curr);
		}
	}

	return search(v);
}
} // namespace

int main() {
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(nullptr);
	std::cout.tie(nullptr);

	int n;
	std::cin >> n;

	std::vector<std::string> words;
	words.reserve(n);

	std::string foo;
	while (n--) {
		std::cin >> foo;
		words.emplace_back(foo);
	}

	std::cout << solve(words) << ENDL;

	return 0;
}
