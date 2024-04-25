#include <iostream>
#include <fstream>
#include <string>
#include <stack>
#include <vector>
#include <algorithm>

using namespace std;

int syntaxErrorScore(char c) {
    switch (c) {
        case ')': return 3;
        case ']': return 57;
        case '}': return 1197;
        case '>': return 25137;
        default: return 0;
    }
}

long autocompleteScore(string s) {
    long score = 0;
    for (char c : s) {
        score *= 5;
        switch (c) {
            case ')': score += 1; break;
            case ']': score += 2; break;
            case '}': score += 3; break;
            case '>': score += 4; break;
        }
    }
    return score;
}

int main() {
    ifstream input("input.txt");
    string line;
    int syntaxErrorScoreTotal = 0;
    vector<long> autocompleteScores;

    while (getline(input, line)) {
        stack<char> chunkStack;
        bool corrupted = false;
        for (char c : line) {
            if (c == '(') chunkStack.push(')');
            else if (c == '[') chunkStack.push(']');
            else if (c == '{') chunkStack.push('}');
            else if (c == '<') chunkStack.push('>');
            else if (chunkStack.empty() || chunkStack.top() != c) {
                syntaxErrorScoreTotal += syntaxErrorScore(c);
                corrupted = true;
                break;
            } else {
                chunkStack.pop();
            }
        }
        if (!corrupted) {
            string autocompleteString;
            while (!chunkStack.empty()) {
                autocompleteString += chunkStack.top();
                chunkStack.pop();
            }
            autocompleteScores.push_back(autocompleteScore(autocompleteString));
        }
    }

    cout << "Syntax error score: " << syntaxErrorScoreTotal << endl;

    sort(autocompleteScores.begin(), autocompleteScores.end());
    long middleScore = autocompleteScores[autocompleteScores.size() / 2];
    cout << "Middle autocomplete score: " << middleScore << endl;

    return 0;
}