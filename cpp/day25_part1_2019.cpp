
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <deque>
#include <map>
#include <set>
#include <regex>
#include <memory>
#include <algorithm>

class Room;

enum class EmulatorStatus { HALTED, OUTPUT, WAITING_FOR_INPUT };

enum class Mode { EXPLORE, NAVIGATE, TEST };

std::map<std::string, std::string> opposite = {
    {"north", "south"}, {"south", "north"}, {"west", "east"}, {"east", "west"}
};

class Room {
public:
    std::string name;
    std::map<std::string, Room*> connections;

    Room(std::string n) : name(std::move(n)) {}
};

class Emulator {
private:
    std::vector<long long> memory;
    long long ip;
    long long relative_base;

    long long powerOf10(int exp) {
        long long res = 1;
        for (int i = 0; i < exp; ++i) res *= 10;
        return res;
    }

    long long getValue(long long addr) {
        if (addr < 0) throw std::runtime_error("Negative memory address.");
        if (addr >= memory.size()) {
            memory.resize(addr + 1, 0);
        }
        return memory[addr];
    }

    void setValue(long long addr, long long val) {
        if (addr < 0) throw std::runtime_error("Negative memory address.");
        if (addr >= memory.size()) {
            memory.resize(addr + 1, 0);
        }
        memory[addr] = val;
    }

    long long getParameter(int offset) {
        long long instruction = getValue(ip);
        long long mode = (instruction / powerOf10(offset + 1)) % 10;
        long long param_val = getValue(ip + offset);

        if (mode == 0) return getValue(param_val);
        if (mode == 1) return param_val;
        if (mode == 2) return getValue(relative_base + param_val);
        throw std::runtime_error("Unknown parameter mode.");
    }

    long long getWriteAddress(int offset) {
        long long instruction = getValue(ip);
        long long mode = (instruction / powerOf10(offset + 1)) % 10;
        long long param_val = getValue(ip + offset);

        if (mode == 0) return param_val;
        if (mode == 2) return relative_base + param_val;
        throw std::runtime_error("Invalid mode for writing.");
    }

public:
    std::deque<long long> input;

    Emulator(const std::vector<long long>& program) : memory(program), ip(0), relative_base(0) {}

    void writeString(const std::string& s) {
        for (char c : s) {
            input.push_back(static_cast<long long>(c));
        }
    }

    std::pair<long long, EmulatorStatus> emulate() {
        while (true) {
            long long opcode = getValue(ip) % 100;

            if (opcode == 1) {
                long long a = getParameter(1);
                long long b = getParameter(2);
                long long c_addr = getWriteAddress(3);
                setValue(c_addr, a + b);
                ip += 4;
            } else if (opcode == 2) {
                long long a = getParameter(1);
                long long b = getParameter(2);
                long long c_addr = getWriteAddress(3);
                setValue(c_addr, a * b);
                ip += 4;
            } else if (opcode == 3) {
                if (input.empty()) {
                    return {0, EmulatorStatus::WAITING_FOR_INPUT};
                }
                long long a_addr = getWriteAddress(1);
                setValue(a_addr, input.front());
                input.pop_front();
                ip += 2;
            } else if (opcode == 4) {
                long long a = getParameter(1);
                ip += 2;
                return {a, EmulatorStatus::OUTPUT};
            } else if (opcode == 5) {
                long long a = getParameter(1);
                long long b = getParameter(2);
                ip = (a != 0) ? b : ip + 3;
            } else if (opcode == 6) {
                long long a = getParameter(1);
                long long b = getParameter(2);
                ip = (a == 0) ? b : ip + 3;
            } else if (opcode == 7) {
                long long a = getParameter(1);
                long long b = getParameter(2);
                long long c_addr = getWriteAddress(3);
                setValue(c_addr, (a < b) ? 1 : 0);
                ip += 4;
            } else if (opcode == 8) {
                long long a = getParameter(1);
                long long b = getParameter(2);
                long long c_addr = getWriteAddress(3);
                setValue(c_addr, (a == b) ? 1 : 0);
                ip += 4;
            } else if (opcode == 9) {
                long long a = getParameter(1);
                relative_base += a;
                ip += 2;
            } else if (opcode == 99) {
                return {0, EmulatorStatus::HALTED};
            } else {
                throw std::runtime_error("Unknown opcode: " + std::to_string(opcode) + " at position " + std::to_string(ip));
            }
        }
    }
};

std::vector<long long> readProgram(const std::string& filename) {
    std::vector<long long> program;
    std::ifstream file(filename);
    std::string segment;
    while (std::getline(file, segment, ',')) {
        program.push_back(std::stoll(segment));
    }
    return program;
}

std::vector<Room*> findPath(Room* from_room, Room* to_room) {
    std::deque<std::pair<Room*, std::vector<Room*>>> q;
    q.push_back({from_room, {from_room}});
    std::set<std::string> visited;
    visited.insert(from_room->name);

    while (!q.empty()) {
        auto [current, path] = q.front();
        q.pop_front();

        if (current == to_room) {
            return path;
        }

        for (auto const& [dir, neighbor_room_ptr] : current->connections) {
            if (neighbor_room_ptr && visited.find(neighbor_room_ptr->name) == visited.end()) {
                visited.insert(neighbor_room_ptr->name);
                std::vector<Room*> new_path = path;
                new_path.push_back(neighbor_room_ptr);
                q.push_back({neighbor_room_ptr, new_path});
            }
        }
    }
    return {};
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<long long> program = readProgram("input.txt");
    Emulator emulator(program);

    auto sendCommand = [&](const std::string& cmd) {
        emulator.writeString(cmd + "\n");
    };

    static const std::regex roomNameRegex("^== (.+) ==$");
    static const std::regex listItemRegex("^- (.+)$");
    static const std::regex takenRegex("^You take the (.+)\\.$");
    static const std::regex droppedRegex("^You drop the (.+)\\.$");
    static const std::regex resultRegex("\"Oh, hello! You should be able to get in by typing (\\d+) on the keypad at the main airlock\\.\"$");
    static const std::set<std::string> itemBlacklist = {
        "photons", "escape pod", "molten lava", "infinite loop", "giant electromagnet"
    };

    std::map<std::string, std::unique_ptr<Room>> room_storage;
    std::map<std::string, bool> inventory;
    Mode mode = Mode::EXPLORE;
    std::vector<Room*> path;
    Room* checkpoint = nullptr;
    Room* floor_room = nullptr;
    std::string test_dir = "";
    std::vector<std::string> available_items;
    int item_mask = 0;
    Room* last_room = nullptr;
    std::vector<std::string> last_items;
    std::string last_dir = "";
    std::string output_builder;
    Room* current_room = nullptr;

    while (true) {
        auto [char_val, status] = emulator.emulate();

        if (status == EmulatorStatus::HALTED) {
            std::string output = output_builder;
            std::stringstream ss(output);
            std::string line;
            while (std::getline(ss, line)) {
                std::smatch matches;
                if (std::regex_search(line, matches, resultRegex)) {
                    std::cout << matches[1].str() << std::endl;
                    return 0;
                }
            }
            break;
        } else if (status == EmulatorStatus::OUTPUT) {
            output_builder.push_back(static_cast<char>(char_val));
        } else if (status == EmulatorStatus::WAITING_FOR_INPUT) {
            std::string output = output_builder;
            output_builder.clear();

            std::vector<std::string> current_items_in_room_processed_state;
            std::stringstream ss(output);
            std::string line;
            size_t i = 0;
            std::vector<std::string> lines;
            while(std::getline(ss, line)) {
                lines.push_back(line);
            }

            bool current_room_updated_by_taken_dropped = false;

            while (i < lines.size()) {
                std::string current_line = lines[i];
                std::smatch matches;

                if (current_line.empty() || current_line == "Command?") {
                    i++;
                    continue;
                }

                if (std::regex_search(current_line, matches, roomNameRegex)) {
                    std::string name = matches[1].str();
                    i++;
                    while (i < lines.size() && !lines[i].empty()) { i++; }
                    if (room_storage.find(name) == room_storage.end()) {
                        room_storage[name] = std::make_unique<Room>(name);
                    }
                    current_room = room_storage[name].get();
                    current_items_in_room_processed_state.clear();
                    current_room_updated_by_taken_dropped = false;
                    continue;
                }

                if (current_line == "Doors here lead:") {
                    i++;
                    while (i < lines.size() && !lines[i].empty()) {
                        std::string door_line = lines[i];
                        if (std::regex_search(door_line, matches, listItemRegex)) {
                            std::string direction = matches[1].str();
                            if (current_room && current_room->connections.find(direction) == current_room->connections.end()) {
                                current_room->connections[direction] = nullptr;
                            }
                        }
                        i++;
                    }
                    continue;
                }

                if (current_line == "Items here:") {
                    i++;
                    while (i < lines.size() && !lines[i].empty()) {
                        std::string item_line = lines[i];
                        if (std::regex_search(item_line, matches, listItemRegex)) {
                            std::string item = matches[1].str();
                            if (!current_room_updated_by_taken_dropped) {
                                current_items_in_room_processed_state.push_back(item);
                            }
                        }
                        i++;
                    }
                    continue;
                }

                if (std::regex_search(current_line, matches, takenRegex)) {
                    std::string taken = matches[1].str();
                    inventory[taken] = true;
                    if (last_room) {
                        current_room = last_room;
                        current_items_in_room_processed_state = last_items;
                        auto it = std::remove(current_items_in_room_processed_state.begin(), current_items_in_room_processed_state.end(), taken);
                        current_items_in_room_processed_state.erase(it, current_items_in_room_processed_state.end());
                        current_room_updated_by_taken_dropped = true;
                    }
                    i++;
                    continue;
                }

                if (std::regex_search(current_line, matches, droppedRegex)) {
                    std::string dropped = matches[1].str();
                    inventory[dropped] = false;
                    if (last_room) {
                        current_room = last_room;
                        current_items_in_room_processed_state = last_items;
                        current_items_in_room_processed_state.push_back(dropped);
                        current_room_updated_by_taken_dropped = true;
                    }
                    i++;
                    continue;
                }

                if (current_line.rfind("A loud, robotic voice says \"Alert!", 0) == 0) {
                    if (mode == Mode::EXPLORE) {
                        if (!path.empty()) {
                            path.pop_back();
                        }
                        checkpoint = last_room;
                        floor_room = current_room;
                        test_dir = last_dir;
                        if (checkpoint && !test_dir.empty()) {
                            checkpoint->connections[test_dir] = floor_room;
                        }
                    }
                    last_room = nullptr;
                    last_items.clear();
                    last_dir = "";
                    i++;
                    continue;
                }
                i++;
            }

            if (last_room && !last_dir.empty() && current_room) {
                if (last_room->connections.find(last_dir) == last_room->connections.end() || last_room->connections[last_dir] == nullptr) {
                    last_room->connections[last_dir] = current_room;
                    current_room->connections[opposite[last_dir]] = last_room;
                }
            }

            last_room = current_room;
            last_items = current_items_in_room_processed_state;
            last_dir = "";

            if (mode == Mode::EXPLORE) {
                bool item_action_taken = false;
                for (const auto& item : current_items_in_room_processed_state) {
                    if (itemBlacklist.find(item) == itemBlacklist.end()) {
                        sendCommand("take " + item);
                        item_action_taken = true;
                        break;
                    }
                }
                if (item_action_taken) {
                    continue;
                }

                std::string target_direction = "";
                for (const auto& [direction, room_ptr] : current_room->connections) {
                    if (room_ptr == nullptr) {
                        path.push_back(current_room);
                        target_direction = direction;
                        break;
                    }
                }

                if (!target_direction.empty()) {
                    last_dir = target_direction;
                    sendCommand(target_direction);
                    continue;
                }

                if (!path.empty()) {
                    Room* last_p_room = path.back();
                    path.pop_back();

                    std::string back_dir = "";
                    for (const auto& [direction, room_ptr] : current_room->connections) {
                        if (room_ptr == last_p_room) {
                            back_dir = direction;
                            break;
                        }
                    }
                    if (!back_dir.empty()) {
                        last_dir = back_dir;
                        sendCommand(back_dir);
                        continue;
                    } else {
                        throw std::runtime_error("Cannot go from \"" + current_room->name + "\" to \"" + last_p_room->name + "\"");
                    }
                }

                if (checkpoint && floor_room) {
                    std::vector<Room*> new_path = findPath(current_room, checkpoint);
                    if (!new_path.empty()) {
                        path.assign(new_path.begin() + 1, new_path.end());
                    }
                    mode = Mode::NAVIGATE;
                    continue;
                }

            } else if (mode == Mode::NAVIGATE) {
                if (!path.empty()) {
                    Room* next_room = path[0];
                    path.erase(path.begin());

                    std::string direction = "";
                    for (const auto& [dir, room_ptr] : current_room->connections) {
                        if (room_ptr == next_room) {
                            direction = dir;
                            break;
                        }
                    }
                    if (!direction.empty()) {
                        last_dir = direction;
                        sendCommand(direction);
                        continue;
                    } else {
                        throw std::runtime_error("Cannot go from \"" + current_room->name + "\" to \"" + next_room->name + "\"");
                    }
                } else {
                    available_items.clear();
                    for (const auto& [item, has] : inventory) {
                        if (has) {
                            available_items.push_back(item);
                        }
                    }
                    std::sort(available_items.begin(), available_items.end());
                    item_mask = 0;
                    mode = Mode::TEST;
                    continue;
                }
            } else if (mode == Mode::TEST) {
                bool item_action_taken = false;
                for (size_t index = 0; index < available_items.size(); ++index) {
                    std::string item = available_items[index];
                    bool target_state = ((item_mask >> index) & 1) != 0;
                    if (inventory[item] != target_state) {
                        std::string action = target_state ? "take" : "drop";
                        sendCommand(action + " " + item);
                        item_action_taken = true;
                        break;
                    }
                }

                if (item_action_taken) {
                    continue;
                } else {
                    item_mask++;
                    if (!test_dir.empty()) {
                        sendCommand(test_dir);
                        continue;
                    } else {
                        throw std::runtime_error("Test direction (test_dir) is not set.");
                    }
                }
            }
        }
    }
    return 0;
}
