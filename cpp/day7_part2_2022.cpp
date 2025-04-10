
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

int main() {
    map<string, int> dirs;
    map<string, int> files;
    vector<string> curr;
    curr.push_back("");
    dirs[""] = 0;

    ifstream file("input.txt");
    string line;

    while (getline(file, line)) {
        stringstream ss(line);
        string txt[3];
        ss >> txt[0];

        if (txt[0] == "$") {
            ss >> txt[1];
            if (txt[1] == "cd") {
                ss >> txt[2];
                if (txt[2] == "/") {
                    curr.clear();
                    curr.push_back("");
                } else if (txt[2] == "..") {
                    if (curr.size() > 1) {
                        curr.pop_back();
                    }
                } else {
                    curr.push_back(txt[2]);
                }
                string path;
                for (size_t i = 0; i < curr.size(); ++i) {
                    path += curr[i];
                    if (i < curr.size() - 1) path += "/";
                }
                dirs[path] = 0;
            }
        } else {
            if (txt[0] != "dir") {
                int fileSize = stoi(txt[0]);
                ss >> txt[1];
                string path;
                for (size_t i = 0; i < curr.size(); ++i) {
                    path += curr[i];
                    if (i < curr.size() - 1) path += "/";
                }
                files[path + "/" + txt[1]] = fileSize;
            }
        }
    }

    for (auto const& [f, s] : files) {
        string path = f;
        size_t pos = path.find_last_of("/");
        while (pos != string::npos) {
            path = path.substr(0, pos);
            dirs[path] += s;
            pos = path.find_last_of("/");
        }
    }

    vector<int> sizes;
    for (auto const& [dir, size] : dirs) {
        sizes.push_back(size);
    }
    sort(sizes.begin(), sizes.end());

    int total = 70000000;
    int want = 30000000;
    int available = total - dirs[""];

    auto it = lower_bound(sizes.begin(), sizes.end(), want - available);
    cout << *it << endl;

    return 0;
}
