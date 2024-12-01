#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <sstream>
#include <cstdlib>

using namespace std;

int main() {
    ifstream file("input.txt");
    string line;
    vector<int> left_list, right_list;
    
    while (getline(file, line)) {
        if (line.empty()) continue;
        
        istringstream iss(line);
        int left, right;
        iss >> left >> right;
        
        left_list.push_back(left);
        right_list.push_back(right);
    }
    
    sort(left_list.begin(), left_list.end());
    sort(right_list.begin(), right_list.end());
    
    long total_distance = 0;
    for (size_t i = 0; i < left_list.size(); i++) {
        total_distance += abs(left_list[i] - right_list[i]);
    }
    
    cout << "Total distance: " << total_distance << endl;
    return 0;
}
