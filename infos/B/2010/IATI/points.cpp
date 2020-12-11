#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = long long;

/*
Bruteforce computation of all possible sums of subsets
*/
template <class Iterator>
auto all_sums(Iterator first, Iterator last) {
    std::vector<num> results;

    std::function<void(Iterator, num)> recurse;
    recurse = [&](auto curr, auto total) {
        if (curr == last) {
            results.emplace_back(total);
            return;
        }

        recurse(std::next(curr), total);
        recurse(std::next(curr), total + *curr);
    };

    recurse(first, 0);

    return results;
}

/*
Meet in the Middle solution
Bruteforce all sums for both halves of the array, then merge
*/
template <class Iterator>
auto solve(Iterator first, Iterator last, num target) {
    auto dist = std::distance(first, last);
    auto mid = std::next(first, dist / 2);

    auto first_half = all_sums(first, mid);
    auto second_half = all_sums(mid, last);

    std::sort(second_half.begin(), second_half.end());

    num res = 0;
    for (auto it = first_half.begin(); it != first_half.end(); ++it) {
        auto p = std::lower_bound(second_half.begin(), second_half.end(),
                                  target - *it);

        res += std::distance(p, second_half.end());
    }

    return res;
}
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    num n, t;
    std::cin >> n >> t;

    std::vector<num> v;
    v.reserve(n);

    num foo;
    while (n--) {
        std::cin >> foo;
        v.emplace_back(foo);
    }

    std::cout << solve(v.begin(), v.end(), t) << ENDL;

    return 0;
}