
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <map>
#include <algorithm>

using namespace std;

struct Valve {
    string id;
    int flow;
    map<string, int> tunnels;
};

int maxPressure(map<string, Valve>& valves, string curr, int minute, int pressure, vector<string>& open, int d) {
    int max_pressure = pressure;
    for (size_t i = 0; i < open.size(); ++i) {
        string next = open[i];
        vector<string> new_open;
        for (size_t j = 0; j < open.size(); ++j) {
            if (open[j] != next) {
                new_open.push_back(open[j]);
            }
        }
        int time_left = minute - valves[curr].tunnels[next] - 1;
        if (time_left > 0) {
            max_pressure = max(max_pressure, maxPressure(valves, next, time_left, time_left * valves[next].flow + pressure, new_open, d + 1));
        }
    }
    return max_pressure;
}

int main() {
    map<string, Valve> valves;
    ifstream file("input.txt");
    string line;

    while (getline(file, line)) {
        Valve v;
        stringstream ss(line);
        string token;
        ss >> token >> v.id >> token >> token >> token;
        v.flow = stoi(token.substr(5, token.size() - 6));
        ss >> token >> token >> token;
        if (token.back() == 's') {
            token = token.substr(0, token.size() - 1);
        }
        ss >> token;
        v.tunnels[v.id] = 0;
        while (ss >> token) {
            if (token.back() == ',') {
                token = token.substr(0, token.size() - 1);
            }
            v.tunnels[token] = 1;
        }
        valves[v.id] = v;
    }

    for (auto const& [k, kv] : valves) {
        for (auto const& [i, iv] : valves) {
            for (auto const& [j, jv] : valves) {
                if (valves[i].tunnels.count(k) && valves[k].tunnels.count(j)) {
                    int dik = valves[i].tunnels[k];
                    int dkj = valves[k].tunnels[j];
                    if (!valves[i].tunnels.count(j) || valves[i].tunnels[j] > dik + dkj) {
                        valves[i].tunnels[j] = dik + dkj;
                    }
                }
            }
        }
    }

    vector<string> open;
    for (auto const& [id, v] : valves) {
        if (v.flow > 0) {
            open.push_back(id);
        }
    }

    cout << maxPressure(valves, "AA", 30, 0, open, 0) << endl;

    return 0;
}
