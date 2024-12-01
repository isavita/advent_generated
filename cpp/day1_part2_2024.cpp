#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <sstream>

using namespace std;

int main() {
    ifstream file("input.txt");
    string line;
    vector<int> left_list;
    unordered_map<int, int> right_counts;
    
    while (getline(file, line)) {
        if (line.empty()) continue;
        
        istringstream iss(line);
        int left, right;
        iss >> left >> right;
        
        left_list.push_back(left);
        right_counts[right]++;
    }
    
    long similarity_score = 0;
    for (int num : left_list) {
        similarity_score += (long)num * right_counts[num];
    }
    
    cout << "Similarity score: " << similarity_score << endl;
    return 0;
}
