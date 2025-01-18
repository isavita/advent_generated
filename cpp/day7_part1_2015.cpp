
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <regex>
#include <algorithm>

using namespace std;

unsigned short memoDFS(const map<string, string>& graph, const string& entry, map<string, unsigned short>& memo) {
    if (memo.count(entry)) {
        return memo[entry];
    }

    regex numRegex("[0-9]+");
    if (regex_match(entry, numRegex)) {
        return static_cast<unsigned short>(stoi(entry));
    }

    string sourceRule = graph.at(entry);
    vector<string> parts;
    stringstream ss(sourceRule);
    string part;
    while (ss >> part) {
        parts.push_back(part);
    }

    unsigned short result;
    if (parts.size() == 1) {
        result = memoDFS(graph, parts[0], memo);
    } else if (parts[0] == "NOT") {
        result = ~memoDFS(graph, parts[1], memo);
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

    string line;
    map<string, string> wireToRule;
    while (getline(file, line)) {
        size_t pos = line.find(" -> ");
        if (pos != string::npos) {
            wireToRule[line.substr(pos + 4)] = line.substr(0, pos);
        }
    }
    file.close();

    map<string, unsigned short> memo;
    cout << memoDFS(wireToRule, "a", memo) << endl;

    return 0;
}
