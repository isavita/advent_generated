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

// Counts the total number of direct and indirect orbits
int countOrbits(string[string] orbitMap) {
    int totalOrbits = 0;

    foreach (object, orbitsAround; orbitMap) {
        string current = orbitsAround;
        while (current != "COM") {
            totalOrbits++;
            current = orbitMap[current]; // Move to the next object in the chain
        }
        totalOrbits++; // Count the orbit around COM itself
    }

    return totalOrbits;
}

void main() {
    auto orbitMap = parseOrbitMap("input.txt");
    int totalOrbits = countOrbits(orbitMap);
    writeln(totalOrbits);
}

