
import std.stdio;
import std.string;
import std.file;
import std.conv;
import std.algorithm;
import std.array;
import std.format;
import std.typecons;

struct Gate {
    string a, op, b;
}

struct GateDef {
    Gate gate;
    string output;
}

GateDef[] parse(string input) {
    GateDef[] gates;
    auto parts = input.split("\n\n");
    if (parts.length != 2) return gates;

    foreach (line; parts[1].split("\n")) {
        if (line.length == 0) continue;
        auto lineParts = line.split(" -> ");
        if (lineParts.length != 2) continue;
        auto gatePart = lineParts[0].split(" ");
        if (gatePart.length != 3) continue;

        gates ~= GateDef(Gate(gatePart[0], gatePart[1], gatePart[2]), lineParts[1]);
    }
    return gates;
}

auto createLookups(const GateDef[] gates) {
    Gate[string] lookup;
    string[string] reverseLookup;

    foreach (g; gates) {
        lookup[g.output] = g.gate;
        string[] inputs = [g.gate.a, g.gate.b];
        inputs.sort();
        auto key = format("%s_%s_%s", inputs[0], g.gate.op, inputs[1]);
        reverseLookup[key] = g.output;
    }
    return tuple(lookup, reverseLookup);
}

string getReverseLookupKey(string a, string op, string b) {
    if (a is null || b is null) return "";
    string[] inputs = [a, b];
    inputs.sort();
    return format("%s_%s_%s", inputs[0], op, inputs[1]);
}

void swap(ref string[][] pairs, ref GateDef[] gates, string a, string b) {
    pairs ~= [a, b];
    foreach (ref g; gates) {
        if (g.output == a) {
            g.output = b;
        } else if (g.output == b) {
            g.output = a;
        }
    }
}

string solution(GateDef[] gates) {
    string[][] pairs;
    size_t numZ = 0;
    foreach (g; gates) {
        if (g.output.startsWith("z")) {
            numZ++;
        }
    }

    while (pairs.length < 4) {
        string adder;
        string carry;
        auto lookups = createLookups(gates);
        auto lookup = lookups[0];
        auto reverseLookup = lookups[1];

        bool swapped = false;
        foreach (i; 0 .. numZ) {
            auto xi = format("x%02d", i);
            auto yi = format("y%02d", i);
            auto zi = format("z%02d", i);

            if (i == 0) {
                auto pAdder = getReverseLookupKey(xi, "XOR", yi) in reverseLookup;
                adder = pAdder ? *pAdder : null;
                auto pCarry = getReverseLookupKey(xi, "AND", yi) in reverseLookup;
                carry = pCarry ? *pCarry : null;
            } else {
                auto pBit = getReverseLookupKey(xi, "XOR", yi) in reverseLookup;
                if (pBit && carry !is null) {
                    auto bit = *pBit;
                    auto pAdder = getReverseLookupKey(bit, "XOR", carry) in reverseLookup;
                    if (pAdder) {
                        adder = *pAdder;
                        auto pC1 = getReverseLookupKey(xi, "AND", yi) in reverseLookup;
                        auto pC2 = getReverseLookupKey(bit, "AND", carry) in reverseLookup;
                        if (pC1 && pC2) {
                            auto c1 = *pC1;
                            auto c2 = *pC2;
                            auto pCarry = getReverseLookupKey(c1, "OR", c2) in reverseLookup;
                            carry = pCarry ? *pCarry : null;
                        } else {
                            carry = null;
                        }
                    } else {
                        adder = null;
                    }
                } else {
                    adder = null;
                }
            }

            if (adder is null) {
                auto pGate = zi in lookup;
                if (pGate && carry !is null) {
                    auto gate = *pGate;
                    auto pBit = getReverseLookupKey(xi, "XOR", yi) in reverseLookup;
                    if (pBit) {
                        auto bit = *pBit;
                        if (getReverseLookupKey(gate.a, "XOR", carry) in reverseLookup) {
                            swap(pairs, gates, bit, gate.a);
                            swapped = true;
                        } else if (getReverseLookupKey(gate.b, "XOR", carry) in reverseLookup) {
                            swap(pairs, gates, bit, gate.b);
                            swapped = true;
                        }
                    }
                }
            } else if (adder != zi) {
                swap(pairs, gates, adder, zi);
                swapped = true;
            }

            if (swapped) {
                break;
            }
        }
    }

    auto flatPairs = pairs.joiner.array;
    flatPairs.sort();
    return flatPairs.join(",");
}

void main() {
    auto gates = parse(readText("input.txt"));
    if (gates.length > 0) {
        solution(gates).writeln;
    }
}
