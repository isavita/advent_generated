import std.stdio;
import std.conv;
import std.file;
import std.array;
import std.algorithm;
import std.string;

// Parses the input file to build the orbit map
string[string] parseOrbitMap(string fileName) {
    string[] lines = readText(fileName).strip().split("\n");
    string[string] orbitMap;

    foreach (line; lines) {
        auto parts = line.split(")");
        orbitMap[parts[1]] = parts[0]; // parts[1] orbits parts[0]
    }

    return orbitMap;
}

// Finds the path from an object to the COM
string[] findPathToCOM(string[string] orbitMap, string start) {
    string[] path;
    string current = start;
    
    while (current in orbitMap) {
        current = orbitMap[current];
        path ~= current;
    }

    return path;
}

// Calculates the minimum number of orbital transfers
int calculateTransfers(string[string] orbitMap, string start, string end) {
    auto pathYOU = findPathToCOM(orbitMap, "YOU");
    auto pathSAN = findPathToCOM(orbitMap, "SAN");

    // Find the first common ancestor
    int commonAncestorIndexYOU = cast(int)pathYOU.countUntil!(a => pathSAN.canFind(a));
    int commonAncestorIndexSAN = cast(int)pathSAN.countUntil!(a => pathYOU.canFind(a));

    // Calculate the number of steps to the common ancestor for both YOU and SAN
    return commonAncestorIndexYOU + commonAncestorIndexSAN;
}

void main() {
    auto orbitMap = parseOrbitMap("input.txt");
    int transfers = calculateTransfers(orbitMap, "YOU", "SAN");
    writeln(transfers);
}
