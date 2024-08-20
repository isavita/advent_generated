import std.stdio;
import std.file;
import std.array;
import std.string;
import std.conv;
import std.algorithm;

struct Replacement {
    string from;
    string to;
}

void main() {
    auto replacements = readReplacements("input.txt");
    string molecule = readMolecule("input.txt");
    auto distinctMolecules = generateDistinctMolecules(molecule, replacements);
    writeln(distinctMolecules.length);
}

Replacement[] readReplacements(string filename) {
    Replacement[] replacements;
    auto file = File(filename, "r");
    foreach (line; file.byLine()) {
        if (line == "") {
            break;
        }
        auto parts = line.split(" => ");
        replacements ~= Replacement(parts[0].idup, parts[1].idup);
    }
    return replacements;
}

string readMolecule(string filename) {
    auto file = File(filename, "r");
    foreach (line; file.byLine()) {
        if (line == "") {
            return file.byLine().front.to!string.idup;
        }
    }
    return "";
}

string[] generateDistinctMolecules(string molecule, Replacement[] replacements) {
    string[] molecules;
    foreach (replacement; replacements) {
        size_t pos = 0;
        while ((pos = molecule.indexOf(replacement.from, pos)) != -1) {
            auto newMolecule = molecule[0 .. pos] ~ replacement.to ~ molecule[pos + replacement.from.length .. $];
            if (!molecules.canFind(newMolecule)) {
                molecules ~= newMolecule;
            }
            pos++;
        }
    }
    return molecules;
}