
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

int main() {
  ifstream inputFile("input.txt");
  if (!inputFile.is_open()) {
    cerr << "Error opening input.txt" << endl;
    return 1;
  }

  vector<string> schematic;
  string line;
  while (getline(inputFile, line)) {
    schematic.push_back(line);
  }

  int sumOfPartNumbers = 0;
  int sumOfGearRatios = 0;

  int rows = schematic.size();
  int cols = schematic[0].size();

  for (int i = 0; i < rows; ++i) {
    for (int j = 0; j < cols; ++j) {
      if (schematic[i][j] == '*') {
        vector<int> adjacentNumbers;

        // Check all 8 adjacent positions
        for (int x = max(0, i - 1); x <= min(rows - 1, i + 1); ++x) {
          for (int y = max(0, j - 1); y <= min(cols - 1, j + 1); ++y) {
            if (isdigit(schematic[x][y])) {
              // Extract the full number
              int start = y;
              while (start > 0 && isdigit(schematic[x][start - 1])) {
                start--;
              }

              int end = y;
              while (end < cols - 1 && isdigit(schematic[x][end + 1])) {
                end++;
              }

              string numberStr = schematic[x].substr(start, end - start + 1);
              int number = stoi(numberStr);
              adjacentNumbers.push_back(number);

              // Replace the number with periods to avoid counting it again
              for (int k = start; k <= end; ++k) {
                schematic[x][k] = '.';
              }
              
            }
          }
        }

        // Remove duplicates from adjacentNumbers
        vector<int> uniqueNumbers;
        for (int num : adjacentNumbers) {
            bool found = false;
            for (int uniqueNum : uniqueNumbers) {
                if (num == uniqueNum) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                uniqueNumbers.push_back(num);
            }
        }
        
        if (uniqueNumbers.size() == 2) {
          sumOfGearRatios += uniqueNumbers[0] * uniqueNumbers[1];
        }
      }
    }
  }

  // Reset schematic for part 1 processing
    inputFile.clear();
    inputFile.seekg(0, ios::beg);
    schematic.clear();
    while (getline(inputFile, line)) {
        schematic.push_back(line);
    }
    
    for (int i = 0; i < rows; ++i) {
        for (int j = 0; j < cols; ++j) {
            if (isdigit(schematic[i][j])) {
                int start = j;
                while (start > 0 && isdigit(schematic[i][start - 1])) {
                    start--;
                }

                int end = j;
                while (end < cols - 1 && isdigit(schematic[i][end + 1])) {
                    end++;
                }

                bool isPartNumber = false;
                for (int x = max(0, i - 1); x <= min(rows - 1, i + 1); ++x) {
                    for (int y = max(0, start - 1); y <= min(cols - 1, end + 1); ++y) {
                        if (!isdigit(schematic[x][y]) && schematic[x][y] != '.') {
                            isPartNumber = true;
                            break;
                        }
                    }
                    if (isPartNumber) break;
                }

                if (isPartNumber) {
                    string numberStr = schematic[i].substr(start, end - start + 1);
                    sumOfPartNumbers += stoi(numberStr);
                    j = end;
                } else {
                    j = end;
                }
            }
        }
    }


  cout << "Sum of part numbers: " << sumOfPartNumbers << endl;
  cout << "Sum of gear ratios: " << sumOfGearRatios << endl;

  inputFile.close();
  return 0;
}
