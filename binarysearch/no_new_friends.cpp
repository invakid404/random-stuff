#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
template <class T>
using vec = std::vector<T>;

template <class T>
using mat = vec<vec<T>>;

class Solution {
public:
    bool solve(int n, mat<int>& friends) {
        std::vector<bool> has_friend(n);

        for (auto& friend_pair : friends) {
            auto a = friend_pair[0];
            auto b = friend_pair[1];

            has_friend[a] = has_friend[b] = true;
        }

        return std::find(has_friend.begin(), has_friend.end(), false) ==
               has_friend.end();
    }
};
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n, k;
    std::cin >> n >> k;

    mat<int> friends(k, vec<int>(2));
    for (auto& row : friends) {
        for (auto& val : row) {
            std::cin >> val;
        }
    }

    Solution solution;
    std::cout << solution.solve(n, friends) << ENDL;

    return 0;
}