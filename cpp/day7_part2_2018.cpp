
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <queue>
#include <limits>

int taskDuration(char task, int baseTime) {
    return baseTime + (task - 'A') + 1;
}

int totalTimeUntilCompletion(const std::string& filename, int workerCount, int baseTime) {
    std::map<char, std::vector<char>> tasks;
    std::map<char, int> indegree;
    std::set<char> allTasks;

    std::ifstream file(filename);
    std::string line;
    while (std::getline(file, line)) {
        if (line.length() >= 37) {
            char prereq = line[5];
            char step = line[36];
            tasks[prereq].push_back(step);
            indegree[step]++;
            allTasks.insert(prereq);
            allTasks.insert(step);
        }
    }
    file.close();

    for (char task : allTasks) {
        if (indegree.find(task) == indegree.end()) {
            indegree[task] = 0;
        }
    }

    std::priority_queue<char, std::vector<char>, std::greater<char>> availableTasks;
    for (auto const& [task, degree] : indegree) {
        if (degree == 0) {
            availableTasks.push(task);
        }
    }

    std::map<char, int> inProgress;
    int timeElapsed = 0;

    while (!availableTasks.empty() || !inProgress.empty()) {
        while (!availableTasks.empty() && inProgress.size() < workerCount) {
            char currentTask = availableTasks.top();
            availableTasks.pop();
            inProgress[currentTask] = taskDuration(currentTask, baseTime);
        }

        if (inProgress.empty()) {
            break;
        }

        int minTimeAdvance = std::numeric_limits<int>::max();
        for (auto const& [task, timeLeft] : inProgress) {
            minTimeAdvance = std::min(minTimeAdvance, timeLeft);
        }
        
        timeElapsed += minTimeAdvance;

        std::vector<char> completedTasks;
        auto it = inProgress.begin();
        while (it != inProgress.end()) {
            it->second -= minTimeAdvance;
            if (it->second <= 0) {
                completedTasks.push_back(it->first);
                it = inProgress.erase(it);
            } else {
                ++it;
            }
        }

        for (char task : completedTasks) {
            if (tasks.count(task)) { 
                for (char nextTask : tasks[task]) {
                    indegree[nextTask]--;
                    if (indegree[nextTask] == 0) {
                        availableTasks.push(nextTask);
                    }
                }
            }
        }
    }

    return timeElapsed;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::cout << totalTimeUntilCompletion("input.txt", 5, 60) << std::endl;

    return 0;
}
