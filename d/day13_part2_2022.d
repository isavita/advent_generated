import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.json; // Added import for JSON parsing

// Function to parse a single packet
auto parsePacket(string line) {
    // Use JSONValue to parse JSON-like structures
    auto parsed = parseJSON(line);
    return parsed;
}

// Function to compare two packets
int comparePackets(JSONValue left, JSONValue right) {
    if (left.type == JSON_TYPE.ARRAY && right.type == JSON_TYPE.ARRAY) {
        for (size_t i = 0; i < left.array.length && i < right.array.length; i++) {
            int cmp = comparePackets(left[i], right[i]);
            if (cmp != 0) {
                return cmp;
            }
        }
        return cast(int)(left.array.length - right.array.length);
    } else if (left.type == JSON_TYPE.ARRAY) {
        return comparePackets(left, JSONValue([right]));
    } else if (right.type == JSON_TYPE.ARRAY) {
        return comparePackets(JSONValue([left]), right);
    } else {
        return cast(int)(left.integer - right.integer);
    }
}

void main() {
    auto file = File("input.txt", "r");
    string[] lines;
    foreach (line; file.byLine()) {
        lines ~= line.idup;
    }
    file.close();

    JSONValue[] packets;
    foreach (line; lines) {
        if (line.length == 0) {
            continue;
        }
        packets ~= parsePacket(line);
    }

    packets ~= JSONValue([JSONValue([2])]);
    packets ~= JSONValue([JSONValue([6])]);

    packets.sort!((a, b) => comparePackets(a, b) < 0);

    int index2 = 0;
    int index6 = 0;
    foreach (i, packet; packets) {
        if (packet.toString() == "[[2]]") {
            index2 = cast(int)i + 1;
        } else if (packet.toString() == "[[6]]") {
            index6 = cast(int)i + 1;
        }
    }

    writeln(index2 * index6);
}