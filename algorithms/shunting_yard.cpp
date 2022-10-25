#include <bits/stdc++.h>

namespace {
class Token {
public:
    enum Kind { Number, Operator, LeftParenthesis, RightParenthesis, End };

    explicit Token(Kind kind) : m_kind(kind) {}
    Token(Kind kind, std::string_view&& value)
        : m_kind(kind), m_value(std::move(value)) {}

    auto kind() { return m_kind; }

    auto is(Kind kind) { return m_kind == kind; }

    auto value() { return m_value; }

private:
    Kind m_kind{};
    std::string_view m_value{};
};

class LexerException : public std::exception {
public:
    LexerException(size_t index, char value) : m_index(index), m_value(value) {}

    auto what() { return "Unrecognized character"; }

    auto index() { return m_index; }

    auto value() { return m_value; }

private:
    size_t m_index{};
    char m_value{};
};

using Number = double;

using Operation = std::function<Number(Number, Number)>;

struct Operator {
    enum Associativity { None, LeftToRight, RightToLeft };

    int precedence;
    Associativity associativity;
    Operation operation;
};

struct StringHash {
    using is_transparent = void;

    [[nodiscard]] size_t operator()(std::string_view txt) const {
        return std::hash<std::string_view>{}(txt);
    }

    [[nodiscard]] size_t operator()(std::string const& txt) const {
        return std::hash<std::string>{}(txt);
    }
};

std::unordered_map<std::string, Operator, StringHash, std::equal_to<>>
    operators = {
        {"+", {2, Operator::Associativity::LeftToRight, std::plus<>()}},
        {"-", {2, Operator::Associativity::LeftToRight, std::minus<>()}},
        {"*", {3, Operator::Associativity::LeftToRight, std::multiplies<>()}},
        {"/", {3, Operator::Associativity::LeftToRight, std::divides<>()}},
        {"^",
         {4, Operator::Associativity::RightToLeft,
          [](Number x, Number y) { return std::pow(x, y); }}},
};

class Lexer {
public:
    explicit Lexer(std::string&& value) : m_value(std::move(value)) {
        m_head = m_value.begin();
    }

    auto has() -> bool { return peek() != '\0'; }

    auto next() -> Token {
        while (std::isspace(peek())) {
            take();
        }

        switch (peek()) {
        case '\0':
            return Token(Token::Kind::End);
        case '0' ... '9':
            return number_token();
        case '+':
        case '-':
        case '*':
        case '/':
        case '^':
            return operator_token();
        case '(':
            return left_parenthesis_token();
        case ')':
            return right_parenthesis_token();
        }

        throw LexerException(std::distance(m_value.begin(), m_head), *m_head);
    }

private:
    auto peek() -> char {
        if (m_head == m_value.end()) {
            return '\0';
        }

        return *m_head;
    }

    auto take() -> char {
        auto value = peek();
        ++m_head;

        return value;
    }

    auto number_token() -> Token {
        auto begin = m_head;
        while (std::isdigit(peek())) {
            take();
        }

        return Token(Token::Kind::Number, std::string_view(begin, m_head));
    }

    auto operator_token() -> Token {
        auto begin = m_head;
        take();

        return Token(Token::Kind::Operator, std::string_view(begin, m_head));
    }

    auto left_parenthesis_token() -> Token {
        auto begin = m_head;
        take();

        return Token(Token::Kind::LeftParenthesis,
                     std::string_view(begin, m_head));
    }

    auto right_parenthesis_token() -> Token {
        auto begin = m_head;
        take();

        return Token(Token::Kind::RightParenthesis,
                     std::string_view(begin, m_head));
    }

    std::string m_value{};
    std::string::iterator m_head{};
};

class MismatchedParenthesisException : public std::exception {
public:
    auto what() { return "Mismatched parenthesis"; }
};

auto shunting_yard(Lexer& lexer) {
    std::queue<Token> output;
    std::stack<Token> operator_stack;

    while (lexer.has()) {
        auto token = lexer.next();

        switch (token.kind()) {
        case Token::Kind::Number: {
            output.emplace(token);

            break;
        }
        case Token::Kind::Operator: {
            auto operator_data = operators.find(token.value())->second;

            auto precedence = operator_data.precedence;
            auto associativity = operator_data.associativity;

            while (!operator_stack.empty()) {
                auto other_operator = operator_stack.top();
                if (other_operator.is(Token::Kind::LeftParenthesis)) {
                    break;
                }

                auto other_operator_data =
                    operators.find(other_operator.value())->second;
                auto other_precedence = other_operator_data.precedence;

                if (other_precedence < precedence ||
                    associativity != Operator::Associativity::LeftToRight) {
                    break;
                }

                operator_stack.pop();
                output.emplace(other_operator);
            }

            operator_stack.emplace(token);

            break;
        }
        case Token::Kind::LeftParenthesis: {
            operator_stack.emplace(token);

            break;
        }
        case Token::Kind::RightParenthesis: {
            while (true) {
                if (operator_stack.empty()) {
                    throw MismatchedParenthesisException();
                }

                auto current_operator = operator_stack.top();
                if (current_operator.is(Token::Kind::LeftParenthesis)) {
                    break;
                }

                operator_stack.pop();
                output.emplace(current_operator);
            }

            if (!operator_stack.top().is(Token::Kind::LeftParenthesis)) {
                throw MismatchedParenthesisException();
            }

            operator_stack.pop();

            break;
        }
        case Token::Kind::End:
            break;
        }
    }

    while (!operator_stack.empty()) {
        output.emplace(operator_stack.top());
        operator_stack.pop();
    }

    return output;
}

auto evaluate(std::string&& expression) {
    Lexer lexer(std::move(expression));

    auto tokens = shunting_yard(lexer);

    std::stack<Number> values;

    while (!tokens.empty()) {
        auto token = tokens.front();
        tokens.pop();

        if (token.is(Token::Kind::Operator)) {
            auto operator_data = operators.find(token.value())->second;

            Number y = values.top();
            values.pop();

            Number x = values.top();
            values.pop();

            values.emplace(operator_data.operation(x, y));

            continue;
        }

        assert(token.is(Token::Kind::Number));

        double value;
        std::from_chars(token.value().begin(), token.value().end(), value);

        values.emplace(value);
    }

    return values.top();
}
}  // namespace

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    std::string expression;
    std::getline(std::cin, expression);

    std::cout << evaluate(std::move(expression)) << std::endl;

    return 0;
}
