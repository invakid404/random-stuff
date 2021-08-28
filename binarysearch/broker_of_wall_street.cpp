#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
struct state_t {
    int cash;
    int invest;
};

auto solve(std::vector<int>& prices, int fee) {
    return std::accumulate(
               prices.cbegin() + 1, prices.cend(), state_t{0, -prices[0]},
               [&](auto& state, auto price) {
                   return state_t{
                       std::max(state.cash, state.invest + price - fee),
                       std::max(state.invest, state.cash - price)};
               })
        .cash;
}
}  // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    std::vector<int> prices;
    prices.reserve(n);

    int price;
    while (n--) {
        std::cin >> price;
        prices.emplace_back(price);
    }

    int fee;
    std::cin >> fee;

    std::cout << solve(prices, fee) << ENDL;

    return 0;
}
