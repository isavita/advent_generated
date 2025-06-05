
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <queue>

enum PulseValue {
    Low = 0,
    High = 1
};

enum ModulePrefix {
    None,
    FlipFlop,
    Conjunction
};

struct Pulse {
    PulseValue value;
    std::string fromName;
    std::string toName;
};

struct Module {
    std::string name;
    ModulePrefix prefix;
    std::vector<std::string> destinations;
    bool state;
    std::map<std::string, PulseValue> memory;
};

std::map<std::string, Module> parseInput(const std::vector<std::string>& inputLines) {
    std::map<std::string, Module> modules;

    for (const std::string& line : inputLines) {
        size_t arrowPos = line.find(" -> ");
        std::string modulePart = line.substr(0, arrowPos);
        std::string destinationsPart = line.substr(arrowPos + 4);

        Module module;
        module.state = false;

        if (modulePart[0] == '%') {
            module.prefix = FlipFlop;
            module.name = modulePart.substr(1);
        } else if (modulePart[0] == '&') {
            module.prefix = Conjunction;
            module.name = modulePart.substr(1);
        } else {
            module.prefix = None;
            module.name = modulePart;
        }

        size_t start = 0;
        size_t end = destinationsPart.find(", ");
        while (end != std::string::npos) {
            module.destinations.push_back(destinationsPart.substr(start, end - start));
            start = end + 2;
            end = destinationsPart.find(", ", start);
        }
        module.destinations.push_back(destinationsPart.substr(start));

        modules[module.name] = module;
    }

    for (const auto& pair : modules) {
        const Module& module = pair.second;
        for (const std::string& destName : module.destinations) {
            if (modules.count(destName) && modules[destName].prefix == Conjunction) {
                modules[destName].memory[module.name] = Low;
            }
        }
    }

    return modules;
}

std::pair<long long, long long> pushButton(std::map<std::string, Module>& modules, int numCycles) {
    long long cntLow = 0;
    long long cntHigh = 0;
    std::deque<Pulse> pulseQueue;

    for (int cycle = 0; cycle < numCycles; ++cycle) {
        pulseQueue.push_back({Low, "button", "broadcaster"});

        while (!pulseQueue.empty()) {
            Pulse currentPulse = pulseQueue.front();
            pulseQueue.pop_front();

            if (currentPulse.value == Low) {
                cntLow++;
            } else {
                cntHigh++;
            }

            if (modules.find(currentPulse.toName) == modules.end()) {
                continue;
            }

            Module& module = modules[currentPulse.toName];
            PulseValue newPulseValue;

            if (module.prefix == FlipFlop) {
                if (currentPulse.value == Low) {
                    module.state = !module.state;
                    newPulseValue = module.state ? High : Low;
                } else {
                    continue;
                }
            } else if (module.prefix == Conjunction) {
                module.memory[currentPulse.fromName] = currentPulse.value;

                bool allHigh = true;
                for (const auto& memPair : module.memory) {
                    if (memPair.second == Low) {
                        allHigh = false;
                        break;
                    }
                }
                newPulseValue = allHigh ? Low : High;
            } else {
                newPulseValue = currentPulse.value;
            }

            for (const std::string& destName : module.destinations) {
                pulseQueue.push_back({newPulseValue, module.name, destName});
            }
        }
    }
    return {cntLow, cntHigh};
}

long long solve(const std::vector<std::string>& inputLines) {
    std::map<std::string, Module> modules = parseInput(inputLines);

    int numCycles = 1000;
    std::pair<long long, long long> counts = pushButton(modules, numCycles);

    return counts.first * counts.second;
}

std::vector<std::string> readFile(const std::string& fileName) {
    std::vector<std::string> lines;
    std::ifstream file(fileName);
    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            lines.push_back(line);
        }
        file.close();
    } else {
        std::cerr << "Unable to open file: " << fileName << std::endl;
    }
    return lines;
}

int main() {
    std::vector<std::string> input = readFile("input.txt");
    std::cout << solve(input) << std::endl;
    return 0;
}
