
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <regex>

using namespace std;

long long evaluate_simple(string expression) {
    regex pattern("(\\d+)|(\\+)|(\[*])");
    sregex_iterator it(expression.begin(), expression.end(), pattern);
    sregex_iterator end;

    long long total = stoll(it->str());
    it++;

    while (it != end) {
        string op = it->str();
        it++;
        long long num = stoll(it->str());
        it++;

        if (op == "+") {
            total += num;
        } else {
            total *= num;
        }
    }
    return total;
}

long long evaluate_advanced(string expression) {
    regex pattern("(\\d+)|(\\+)|(\[*])");
    vector<string> parts;
    sregex_iterator it(expression.begin(), expression.end(), pattern);
    sregex_iterator end;

    for (sregex_iterator i = it; i != end; ++i) {
        parts.push_back(i->str());
    }

    while (find(parts.begin(), parts.end(), "+") != parts.end()) {
        auto i = find(parts.begin(), parts.end(), "+");
        int index = distance(parts.begin(), i);
        long long total = stoll(parts[index - 1]) + stoll(parts[index + 1]);
        parts.erase(parts.begin() + index - 1, parts.begin() + index + 2);
        parts.insert(parts.begin() + index - 1, to_string(total));
    }

    long long total = stoll(parts[0]);
    for (size_t i = 1; i < parts.size(); i += 2) {
        total *= stoll(parts[i + 1]);
    }
    return total;
}

long long evaluate_expression(string expression, long long (*evaluate_fn)(string)) {
    size_t start = expression.rfind('(');
    while (start != string::npos) {
        size_t end = start + expression.substr(start).find(')');
        string sub_expression = expression.substr(start + 1, end - start - 1);
        long long result = evaluate_fn(sub_expression);
        expression.replace(start, end - start + 1, to_string(result));
        start = expression.rfind('(');
    }
    return evaluate_fn(expression);
}

int main() {
    ifstream file("input.txt");
    string line;
    vector<string> expressions;

    while (getline(file, line)) {
        expressions.push_back(line);
    }
    file.close();

    long long result_part1 = 0;
    for (const string& expression : expressions) {
        result_part1 += evaluate_expression(expression, evaluate_simple);
    }

    long long result_part2 = 0;
    for (const string& expression : expressions) {
        result_part2 += evaluate_expression(expression, evaluate_advanced);
    }

    cout << result_part1 << endl;
    cout << result_part2 << endl;

    return 0;
}
