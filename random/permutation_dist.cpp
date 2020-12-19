#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
template <class Iterator>
auto merge(Iterator a, Iterator mid, Iterator b) {
    using T = typename std::iterator_traits<Iterator>::value_type;

    auto res = 0;

    std::vector<T> temp;
    temp.reserve(std::distance(a, b));

    auto i = a;
    auto j = mid;
    auto k = b;

    while (i < mid && j < b) {
        if (*i < *j) {
            temp.emplace_back(*i);
            ++i;
        } else {
            temp.emplace_back(*j);
            ++j;

            res += std::distance(i, mid);
        }
    }

    while (i < mid) {
        temp.emplace_back(*i);
        ++i;
    }

    while (j < b) {
        temp.emplace_back(*j);
        ++j;
    }

    std::copy(temp.begin(), temp.end(), a);

    return res;
}

template <class Iterator>
auto merge_sort(Iterator a, Iterator b) -> int {
    auto dist = std::distance(a, b);
    if (dist <= 1) {
        return 0;
    }

    auto mid = std::next(a, dist / 2);

    auto res = 0;

    res += merge_sort(a, mid);
    res += merge_sort(mid, b);

    res += merge(a, mid, b);

    return res;
}
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    std::vector<int> x, y;
    int foo;

    x.reserve(n);
    for (auto i = 0; i < n; ++i) {
        std::cin >> foo;
        x.emplace_back(foo);
    }

    y.reserve(n);
    for (auto i = 0; i < n; ++i) {
        std::cin >> foo;
        y.emplace_back(foo);
    }

    std::vector<int> idx_sub(n + 1);
    for (auto i = 0; i < n; ++i) {
        idx_sub[x[i]] = i + 1;
    }

    for (auto& val : y) {
        val = idx_sub[val];
    }

    std::cout << merge_sort(y.begin(), y.end()) << ENDL;

    return 0;
}