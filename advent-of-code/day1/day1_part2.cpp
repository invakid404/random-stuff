#include <bits/stdc++.h>

#define ENDL '\n'

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    std::ifstream is("input.txt");

    std::vector<int> vec;
    int foo;
    while (is >> foo) {
        vec.emplace_back(foo);
    }

    auto res = -1;
    for (auto i = 0; i < vec.size(); ++i) {
        std::unordered_set<int> complements;
        for (auto j = i + 1; j < vec.size(); ++j) {
            auto complement = 2020 - vec[i] - vec[j];
            if (complements.find(complement) != complements.end()) {
                res = vec[i] * vec[j] * complement;
                break;
            }

            complements.insert(vec[j]);
        }

        if (res != -1) {
            break;
        }
    }

    std::cout << res << ENDL;

    return 0;
}