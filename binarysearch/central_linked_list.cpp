#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class LLNode {
public:
    LLNode(int val) : val(val) {}
    ~LLNode() { delete next; }

    int val;
    LLNode* next{};
};

class Solution {
public:
    auto solve(LLNode* node) {
        auto slow = node;
        auto fast = node;

        while (fast != nullptr && fast->next != nullptr) {
            slow = slow->next;
            fast = fast->next->next;
        }

        return slow->val;
    }
};
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    auto fake_head = new LLNode(-1);

    int n;
    std::cin >> n;

    auto it = fake_head;
    int foo;

    while (n--) {
        std::cin >> foo;

        it->next = new LLNode(foo);
        it = it->next;
    }

    Solution solution;
    std::cout << solution.solve(fake_head->next) << ENDL;

    delete fake_head;

    return 0;
}