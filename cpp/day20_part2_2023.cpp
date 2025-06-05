
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <queue>
#include <memory>
#include <numeric>
#include <algorithm>

enum ModuleType {
    BROADCASTER,
    FLIP_FLOP,
    CONJUNCTION
};

class Module {
public:
    std::string name;
    ModuleType type;
    std::vector<std::string> connectsTo;

    Module(std::string n, ModuleType t, std::vector<std::string> ct)
        : name(std::move(n)), type(t), connectsTo(std::move(ct)) {}

    virtual ~Module() = default;
};

class FlipFlop : public Module {
public:
    bool state;

    FlipFlop(std::string n, std::vector<std::string> ct)
        : Module(std::move(n), FLIP_FLOP, std::move(ct)), state(false) {}
};

class Conjunction : public Module {
public:
    std::map<std::string, bool> watches;

    Conjunction(std::string n, std::vector<std::string> ct)
        : Module(std::move(n), CONJUNCTION, std::move(ct)) {}
};

class Broadcaster : public Module {
public:
    Broadcaster(std::string n, std::vector<std::string> ct)
        : Module(std::move(n), BROADCASTER, std::move(ct)) {}
};

struct State {
    std::string from;
    std::string to;
    bool pulse;
};

std::vector<std::string> split(const std::string& s, const std::string& delimiter) {
    std::vector<std::string> tokens;
    size_t lastPos = 0;
    size_t pos;

    while ((pos = s.find(delimiter, lastPos)) != std::string::npos) {
        std::string token = s.substr(lastPos, pos - lastPos);
        if (!token.empty()) {
            tokens.push_back(token);
        }
        lastPos = pos + delimiter.length();
    }
    std::string lastToken = s.substr(lastPos);
    if (!lastToken.empty()) {
        tokens.push_back(lastToken);
    }
    return tokens;
}

std::unique_ptr<Module> parse_line(const std::string& line) {
    size_t arrow_pos = line.find(" -> ");
    std::string left_part = line.substr(0, arrow_pos);
    std::string right_part = line.substr(arrow_pos + 4);

    std::vector<std::string> connects_to_names = split(right_part, ", ");

    if (left_part == "broadcaster") {
        return std::make_unique<Broadcaster>(left_part, connects_to_names);
    } else if (left_part[0] == '%') {
        return std::make_unique<FlipFlop>(left_part.substr(1), connects_to_names);
    } else {
        return std::make_unique<Conjunction>(left_part.substr(1), connects_to_names);
    }
}

void complete_watches(std::map<std::string, std::unique_ptr<Module>>& connections) {
    for (auto const& [name, module_ptr] : connections) {
        if (module_ptr->type == CONJUNCTION) {
            Conjunction* conj_module = static_cast<Conjunction*>(module_ptr.get());
            for (auto const& [name2, module2_ptr] : connections) {
                for (const std::string& connected_name : module2_ptr->connectsTo) {
                    if (connected_name == conj_module->name) {
                        conj_module->watches[name2] = false;
                    }
                }
            }
        }
    }
}

bool simulate_press(std::map<std::string, std::unique_ptr<Module>>& connections,
                    std::map<std::string, long long>& loop_lengths,
                    long long press_number) {

    std::queue<State> q;
    q.push({"button", "broadcaster", false});

    bool rx_low_pulse_received = false;

    while (!q.empty()) {
        State current_state = q.front();
        q.pop();

        auto it = connections.find(current_state.to);
        if (it == connections.end()) {
            if (current_state.to == "rx" && !current_state.pulse) {
                rx_low_pulse_received = true;
            }
            continue;
        }

        Module* module = it->second.get();
        bool incoming_pulse = current_state.pulse;

        switch (module->type) {
            case BROADCASTER: {
                for (const std::string& next_module_name : module->connectsTo) {
                    q.push({module->name, next_module_name, incoming_pulse});
                }
                break;
            }
            case FLIP_FLOP: {
                FlipFlop* ff_module = static_cast<FlipFlop*>(module);
                if (!incoming_pulse) {
                    ff_module->state = !ff_module->state;
                    bool outgoing_pulse = ff_module->state;
                    for (const std::string& next_module_name : ff_module->connectsTo) {
                        q.push({ff_module->name, next_module_name, outgoing_pulse});
                    }
                }
                break;
            }
            case CONJUNCTION: {
                Conjunction* conj_module = static_cast<Conjunction*>(module);
                conj_module->watches[current_state.from] = incoming_pulse;

                bool all_inputs_high = true;
                for (const auto& pair : conj_module->watches) {
                    if (!pair.second) {
                        all_inputs_high = false;
                        break;
                    }
                }

                bool outgoing_pulse = !all_inputs_high;

                auto it_loop = loop_lengths.find(conj_module->name);
                if (it_loop != loop_lengths.end() && outgoing_pulse && it_loop->second == -1) {
                    it_loop->second = press_number;
                }

                for (const std::string& next_module_name : conj_module->connectsTo) {
                    q.push({conj_module->name, next_module_name, outgoing_pulse});
                }
                break;
            }
        }
    }
    return rx_low_pulse_received;
}

long long calculate_lcm(long long a, long long b) {
    if (a == 0 || b == 0) return 0;
    return std::abs(a * b) / std::gcd(a, b);
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt\n";
        return 1;
    }

    std::map<std::string, std::unique_ptr<Module>> connections;
    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) continue;
        std::unique_ptr<Module> module = parse_line(line);
        connections[module->name] = std::move(module);
    }
    file.close();

    complete_watches(connections);

    std::string rx_feeder_name = "";
    for (auto const& [name, module_ptr] : connections) {
        for (const std::string& connected_name : module_ptr->connectsTo) {
            if (connected_name == "rx") {
                rx_feeder_name = name;
                break;
            }
        }
        if (!rx_feeder_name.empty()) break;
    }

    if (rx_feeder_name.empty()) {
        std::cerr << "Error: No module connects to 'rx'\n";
        return 1;
    }

    Conjunction* rx_feeder_conj = dynamic_cast<Conjunction*>(connections[rx_feeder_name].get());
    if (!rx_feeder_conj) {
        std::cerr << "Error: Module connecting to 'rx' is not a Conjunction or doesn't exist: " << rx_feeder_name << "\n";
        return 1;
    }

    std::map<std::string, long long> loop_lengths;
    for (const auto& pair : rx_feeder_conj->watches) {
        loop_lengths[pair.first] = -1;
    }

    long long press_number = 0;
    while (true) {
        press_number++;
        simulate_press(connections, loop_lengths, press_number);

        bool all_lengths_found = true;
        for (const auto& pair : loop_lengths) {
            if (pair.second == -1) {
                all_lengths_found = false;
                break;
            }
        }

        if (all_lengths_found) {
            break;
        }
    }

    long long result_lcm = 1;
    for (const auto& pair : loop_lengths) {
        result_lcm = calculate_lcm(result_lcm, pair.second);
    }

    std::cout << result_lcm << std::endl;

    return 0;
}
