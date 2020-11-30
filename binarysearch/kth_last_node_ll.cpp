#include <bits/stdc++.h>

#define ENDL '\n'

namespace {
class LLNode {
public:
    int val;
    LLNode* next;

    LLNode(int val) : val(val) {}

    ~LLNode() { delete next; }
};

class Solution {
public:
    auto solve(LLNode* head, int k) {
        auto slow = head, fast = head;

        while (k--) {
            fast = fast->next;
        }

        while (fast->next) {
            slow = slow->next;
            fast = fast->next;
        }

        return slow->val;
    }
};
} // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n;
    std::cin >> n;

    auto dummy_head = new LLNode(-1);

    int foo;
    for (auto it = dummy_head; n; --n, it = it->next) {
        std::cin >> foo;
        it->next = new LLNode(foo);
    }

    int k;
    std::cin >> k;

    Solution solution;
    std::cout << solution.solve(dummy_head->next, k) << ENDL;

    delete dummy_head;

    return 0;
}