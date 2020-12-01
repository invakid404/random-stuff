#include <bits/stdc++.h>

#define ENDL '\n'

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    auto res = 0;
    while (n) {
        n /= 5;
        res += n;
    }

    std::cout << res << ENDL;

    return 0;
}