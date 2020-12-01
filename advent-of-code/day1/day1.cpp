#include <bits/stdc++.h>

#define ENDL '\n'

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    std::ifstream is("input.txt");

    std::unordered_set<int> complements;

    int foo;
    while (is >> foo) {
        auto complement = 2020 - foo;
        if (complements.find(complement) != complements.end()) {
            std::cout << foo * complement << ENDL;
            break;
        }

        complements.insert(foo);
    }

    return 0;
}