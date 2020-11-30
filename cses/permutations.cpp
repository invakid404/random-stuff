#include <bits/stdc++.h>

#define ENDL '\n'

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    switch (n) {
        case 1:
            std::cout << "1" << ENDL;
            break;
        case 2:
        case 3:
            std::cout << "NO SOLUTION" << ENDL;
            break;
        default:
            auto i = 2, j = 1;
            if (n & 1) {
                std::swap(i, j);
            }

            for (; i <= n; i += 2) {
                std::cout << i << " ";
            }

            for (; j <= n - 1; j += 2) {
                std::cout << j << " ";
            }

            std::cout << ENDL;
            break;
    }

    return 0;
}