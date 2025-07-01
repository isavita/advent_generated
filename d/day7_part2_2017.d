
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.range;

struct Program {
    int weight;
    string[] holds;
}

struct DfsResult {
    int totalWeight;
    bool balanced;
}

DfsResult[string] cache;

DfsResult dfs(string name, ref Program[string] programs) {
    if (name in cache) {
        return cache[name];
    }

    auto program = programs[name];
    int currentTotalWeight = program.weight;
    int[int] childTowerWeights;

    struct ChildInfo {
        string name;
        int towerWeight;
    }
    ChildInfo[] childrenInfo;

    foreach (childName; program.holds) {
        auto childResult = dfs(childName, programs);
        if (!childResult.balanced) {
            return DfsResult(0, false);
        }
        currentTotalWeight += childResult.totalWeight;
        childTowerWeights[childResult.totalWeight]++;
        childrenInfo ~= ChildInfo(childName, childResult.totalWeight);
    }

    if (childTowerWeights.length > 1) {
        int unbalancedWeight, targetWeight;
        foreach (w, count; childTowerWeights) {
            if (count == 1) unbalancedWeight = w;
            else targetWeight = w;
        }

        foreach (child; childrenInfo) {
            if (child.towerWeight == unbalancedWeight) {
                auto unbalancedProgram = programs[child.name];
                int diff = targetWeight - unbalancedWeight;
                writeln(unbalancedProgram.weight + diff);
                break;
            }
        }
        return DfsResult(0, false);
    }

    auto result = DfsResult(currentTotalWeight, true);
    cache[name] = result;
    return result;
}

void main() {
    Program[string] programs;
    bool[string] isHeld;

    foreach (line; readText("input.txt").splitLines) {
        if (line.length == 0) continue;
        auto parts = line.split();
        auto name = parts[0];
        auto weight = to!int(parts[1][1 .. $-1]);
        string[] holds;

        if (parts.length > 2) {
            holds = parts[3 .. $].map!(a => a.chomp(",")).array;
            foreach (child; holds) {
                isHeld[child] = true;
            }
        }
        programs[name] = Program(weight, holds);
    }

    string root;
    foreach (name; programs.keys) {
        if (name !in isHeld) {
            root = name;
            break;
        }
    }

    dfs(root, programs);
}
