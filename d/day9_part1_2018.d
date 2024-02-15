
import std.stdio;
import std.conv;
import std.array;
import std.algorithm;

struct Marble {
    int value;
    Marble* prev;
    Marble* next;
}

void main() {
    auto input = readInput("input.txt");
    writeln(playMarbleGame(input[0], input[1]));
}

int[] readInput(string filename) {
    auto file = File(filename, "r");
    auto line = file.readln().split;
    file.close();

    return [to!int(line[0]), to!int(line[6])];
}

int playMarbleGame(int players, int lastMarble) {
    int[] scores = new int[players];
    Marble* current = new Marble(0, null, null);
    current.next = current;
    current.prev = current;

    foreach (marble; 1..lastMarble+1) {
        if (marble % 23 == 0) {
            int player = marble % players;
            foreach_reverse (i; 0..7) {
                current = current.prev;
            }
            scores[player] += marble + current.value;
            current.prev.next = current.next;
            current.next.prev = current.prev;
            current = current.next;
        } else {
            current = current.next;
            auto newMarble = new Marble(marble, current, current.next);
            current.next.prev = newMarble;
            current.next = newMarble;
            current = newMarble;
        }
    }

    return scores.maxElement;
}
