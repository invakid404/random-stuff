#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
using num = long long;

template <class T>
using max_heap = std::priority_queue<T>;

auto solve(std::vector<num>&& prices, num storage_cost) {
    for (auto i = 0; i < prices.size(); ++i) {
        prices[i] -= storage_cost * i;
    }

    num res = 0;
    max_heap<num> price_heap;

    for (num i = prices.size() - 1; i >= 0; --i) {
        if (!price_heap.empty() && price_heap.top() > prices[i]) {
            res += price_heap.top() - prices[i];
            price_heap.pop();

            price_heap.emplace(prices[i]);
        }

        price_heap.emplace(prices[i]);
    }

    return res;
}
}  // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    num n, s;
    std::cin >> n >> s;

    std::vector<num> prices(n);
    for (auto& price : prices) {
        std::cin >> price;
    }

    std::cout << solve(std::move(prices), s) << ENDL;

    return 0;
}
