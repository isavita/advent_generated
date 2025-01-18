
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <sstream>
#include <algorithm>

using namespace std;

int sumNumbers(const string& jsonData, int& pos) {
    int sum = 0;
    if (jsonData[pos] == '[') {
        pos++;
        while (jsonData[pos] != ']') {
            sum += sumNumbers(jsonData, pos);
            if (jsonData[pos] == ',') {
                pos++;
            }
        }
        pos++;
    } else if (jsonData[pos] == '{') {
        pos++;
        map<string, int> objMap;
        while (jsonData[pos] != '}') {
            
            while(jsonData[pos] != ':'){
                pos++;
            }
            pos++;
            
            sum += sumNumbers(jsonData, pos);
            if (jsonData[pos] == ',') {
                pos++;
            }
        }
        pos++;
    } else if (isdigit(jsonData[pos]) || jsonData[pos] == '-') {
        string numStr;
        while (isdigit(jsonData[pos]) || jsonData[pos] == '-') {
            numStr += jsonData[pos];
            pos++;
        }
        sum += stoi(numStr);
    } else if (jsonData[pos] == '"'){
        pos++;
        while(jsonData[pos] != '"'){
            pos++;
        }
        pos++;
    }
    else {
        pos++;
    }
    return sum;
}

int main() {
    ifstream inputFile("input.txt");
    string jsonData, line;
    while (getline(inputFile, line)) {
        jsonData += line;
    }
    inputFile.close();

    int pos = 0;
    cout << sumNumbers(jsonData, pos) << endl;

    return 0;
}
