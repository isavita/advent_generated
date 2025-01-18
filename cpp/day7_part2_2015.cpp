
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <regex>
#include <climits>

using namespace std;

int memoDFS(const map<string, string>& graph, const string& entry, map<string, int>& memo);

int someAssemblyRequired(const string& input) {
    map<string, string> wireToRule;
    stringstream ss(input);
    string line;
    while (getline(ss, line)) {
        size_t pos = line.find(" -> ");
        wireToRule[line.substr(pos + 4)] = line.substr(0, pos);
    }

    map<string, int> memo1;
    int aSignal = memoDFS(wireToRule, "a", memo1);

    wireToRule["b"] = to_string(aSignal);
    map<string, int> memo2;
    return memoDFS(wireToRule, "a", memo2);
}

int memoDFS(const map<string, string>& graph, const string& entry, map<string, int>& memo) {
    if (memo.count(entry)) {
        return memo[entry];
    }

    regex numRegex("[0-9]+");
    if (regex_match(entry, numRegex)) {
        return stoi(entry);
    }

    string sourceRule = graph.at(entry);
    stringstream ss(sourceRule);
    vector<string> parts;
    string part;
    while (ss >> part) {
        parts.push_back(part);
    }

    int result;
    if (parts.size() == 1) {
        result = memoDFS(graph, parts[0], memo);
    } else if (parts[0] == "NOT") {
        result = USHRT_MAX ^ memoDFS(graph, parts[1], memo);
    } else if (parts[1] == "AND") {
        result = memoDFS(graph, parts[0], memo) & memoDFS(graph, parts[2], memo);
    } else if (parts[1] == "OR") {
        result = memoDFS(graph, parts[0], memo) | memoDFS(graph, parts[2], memo);
    } else if (parts[1] == "LSHIFT") {
        result = memoDFS(graph, parts[0], memo) << memoDFS(graph, parts[2], memo);
    } else if (parts[1] == "RSHIFT") {
        result = memoDFS(graph, parts[0], memo) >> memoDFS(graph, parts[2], memo);
    }

    memo[entry] = result;
    return result;
}

int main() {
    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Error opening file" << endl;
        return 1;
    }
    stringstream buffer;
    buffer << file.rdbuf();
    string input = buffer.str();
    cout << someAssemblyRequired(input) << endl;
    return 0;
}
