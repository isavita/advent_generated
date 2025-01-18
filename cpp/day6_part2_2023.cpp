
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <algorithm>

using namespace std;

long long calculateWaysToWinLongRace(long long time, long long record) {
    long long waysToWin = 0;
    long long low = 0, high = time;
    while(low <= high){
        long long mid = low + (high - low) / 2;
        long long distance = mid * (time - mid);
        if(distance > record){
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }
    long long firstWin = low;
    low = 0, high = time;
    while(low <= high){
        long long mid = low + (high - low) / 2;
        long long distance = mid * (time - mid);
        if(distance > record){
            low = mid + 1;
        } else {
            high = mid - 1;
        }
    }
    long long lastWin = high;
    return lastWin - firstWin + 1;
}

int main() {
    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Error opening file" << endl;
        return 1;
    }

    string line;
    long long time = 0, distance = 0;
    
    while (getline(file, line)) {
        if (line.empty()) continue;
        size_t colonPos = line.find(':');
        if (colonPos != string::npos) {
            string values = line.substr(colonPos + 1);
            values.erase(remove(values.begin(), values.end(), ' '), values.end());
            if (time == 0) {
                time = stoll(values);
            } else {
                distance = stoll(values);
            }
        }
    }

    file.close();

    long long waysToWin = calculateWaysToWinLongRace(time, distance);
    cout << waysToWin << endl;

    return 0;
}
