#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
using namespace std;

int main() {
	ifstream file("input.txt");
	if (!file.is_open()) {
		cout << "Error opening file" << endl;
		return 1;
	}

	string line;
	int horizontalPosition = 0;
	int depth = 0;
	int aim = 0;

	while (getline(file, line)) {
		vector<string> command;
		stringstream ss(line);
		string word;
		while (ss >> word) {
			command.push_back(word);
		}

		string direction = command[0];
		int units = stoi(command[1]);

		if (direction == "forward") {
			horizontalPosition += units;
			depth += aim * units;
		} else if (direction == "down") {
			aim += units;
		} else if (direction == "up") {
			aim -= units;
		}
	}

	int product = horizontalPosition * depth;
	cout << product << endl;

	return 0;
}