import std.stdio;
import std.file;
import std.conv;
import std.algorithm;
import std.array;

int[] parseInput(string filename) {
    auto file = File(filename, "r");
    int[] weights;
    foreach (line; file.byLine()) {
        weights ~= to!int(line);
    }
    return weights;
}

bool canPartition(int[] weights, int target, int groups) {
    if (groups == 1) {
        return true;
    }
    foreach (i, w; weights) {
        if (w <= target) {
            auto newWeights = weights[0 .. i] ~ weights[i + 1 .. $];
            if (canPartition(newWeights, target - w, groups - 1)) {
                return true;
            }
        }
    }
    return false;
}

int findMinPackages(int[] weights, int target, int start, int count) {
    if (target == 0) {
        return count;
    }
    foreach (i; cast(int)start .. weights.length) {
        if (weights[i] <= target) {
            auto result = findMinPackages(weights, target - weights[i], cast(int)(i + 1), count + 1);
            if (result != -1) {
                return result;
            }
        }
    }
    return -1;
}

long quantumEntanglement(int[] weights) {
    long qe = 1;
    foreach (w; weights) {
        qe *= w;
    }
    return qe;
}

long findMinQE(int[] weights, int target, int start, int count) {
    if (target == 0) {
        return 1;
    }
    long minQE = long.max;
    foreach (i; cast(int)start .. weights.length) {
        if (weights[i] <= target) {
            auto qe = findMinQE(weights, target - weights[i], cast(int)(i + 1), count - 1);
            if (qe != -1) {
                qe *= weights[i];
                if (qe < minQE) {
                    minQE = qe;
                }
            }
        }
    }
    return minQE == long.max ? -1 : minQE;
}

void main() {
    auto weights = parseInput("input.txt");
    int totalWeight = weights.sum;
    if (totalWeight % 3 != 0) {
        writeln("Cannot divide weights into three equal groups.");
        return;
    }
    int target = totalWeight / 3;
    weights.sort!"a > b";

    int minPackages = findMinPackages(weights, target, 0, 0);
    if (minPackages == -1) {
        writeln("Cannot find a valid configuration.");
        return;
    }

    long minQE = findMinQE(weights, target, 0, minPackages);
    writeln(minQE);
}