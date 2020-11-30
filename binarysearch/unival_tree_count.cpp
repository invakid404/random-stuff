#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class Tree {
public:
    int val;
    Tree* left;
    Tree* right;
};

class Solution {
public:
    auto solve(Tree* root) {
        if (root == nullptr) {
            return 0;
        }

        return solve(root->left) + solve(root->right) +
               static_cast<int>(is_unival(root, root->val));
    }

    auto is_unival(Tree* root, int key) -> bool {
        return root == nullptr || root->val == key &&
                                      is_unival(root->left, key) &&
                                      is_unival(root->right, key);
    }
};
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    // cba to deal with inputting a tree kekw

    return 0;
}