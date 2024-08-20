import std.stdio;
import std.file;
import std.conv;
import std.array;

struct Component {
    int port1;
    int port2;
}

void main() {
    auto components = readComponents("input.txt");
    auto strongest = findStrongestBridge(components, 0, 0);
    writeln(strongest);
}

Component[] readComponents(string filename) {
    Component[] components;
    foreach (line; File(filename).byLine()) {
        auto parts = line.split("/");
        auto port1 = to!int(parts[0]);
        auto port2 = to!int(parts[1]);
        components ~= Component(port1, port2);
    }
    return components;
}

int findStrongestBridge(Component[] components, int currentPort, int currentStrength) {
    int maxStrength = currentStrength;
    foreach (i, component; components) {
        if (component.port1 == currentPort || component.port2 == currentPort) {
            int nextPort = component.port1 == currentPort ? component.port2 : component.port1;
            auto remainingComponents = components[0..i] ~ components[i+1..$];
            int strength = findStrongestBridge(remainingComponents, nextPort, currentStrength + component.port1 + component.port2);
            if (strength > maxStrength) {
                maxStrength = strength;
            }
        }
    }
    return maxStrength;
}