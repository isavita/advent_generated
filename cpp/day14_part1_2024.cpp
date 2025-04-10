
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

struct Robot {
  int x, y, vx, vy;
};

int main() {
  ifstream inputFile("input.txt");
  if (!inputFile.is_open()) {
    cerr << "Error opening input.txt" << endl;
    return 1;
  }

  vector<Robot> robots;
  string line;
  while (getline(inputFile, line)) {
    Robot robot;
    sscanf(line.c_str(), "p=%d,%d v=%d,%d", &robot.x, &robot.y, &robot.vx, &robot.vy);
    robots.push_back(robot);
  }

  inputFile.close();

  const int width = 101;
  const int height = 103;
  const int time = 100;

  vector<Robot> finalPositions;
  for (const auto& robot : robots) {
    Robot finalRobot = robot;
    finalRobot.x = (robot.x + robot.vx * time) % width;
    if (finalRobot.x < 0) finalRobot.x += width; 
    finalRobot.y = (robot.y + robot.vy * time) % height;
    if (finalRobot.y < 0) finalRobot.y += height;
    
    finalPositions.push_back(finalRobot);
  }

  int q1 = 0, q2 = 0, q3 = 0, q4 = 0;
  for (const auto& robot : finalPositions) {
    if (robot.x < width / 2 && robot.y < height / 2) {
      q1++;
    } else if (robot.x > width / 2 && robot.y < height / 2) {
      q2++;
    } else if (robot.x < width / 2 && robot.y > height / 2) {
      q3++;
    } else if (robot.x > width / 2 && robot.y > height / 2) {
      q4++;
    }
  }

  cout << (long long)q1 * q2 * q3 * q4 << endl;

  return 0;
}
