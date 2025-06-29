
import std.stdio;
import std.algorithm;

class Marble {
    long value;
    Marble next;
    Marble prev;

    this(long val) {
        this.value = val;
    }
}

void main() {
    int players;
    int lastMarbleWorth;
    auto file = File("input.txt", "r");
    file.readf("%d players; last marble is worth %d points", &players, &lastMarbleWorth);
    
    long lastMarble = lastMarbleWorth * 100;
    auto scores = new long[players];

    auto current = new Marble(0);
    current.next = current;
    current.prev = current;

    foreach (long marble; 1 .. lastMarble + 1) {
        if (marble % 23 == 0) {
            foreach (_; 0 .. 7) {
                current = current.prev;
            }
            scores[(marble - 1) % players] += marble + current.value;
            
            auto toRemove = current;
            current = toRemove.next;
            toRemove.prev.next = toRemove.next;
            toRemove.next.prev = toRemove.prev;
        } else {
            current = current.next;
            auto newMarble = new Marble(marble);
            newMarble.prev = current;
            newMarble.next = current.next;
            current.next.prev = newMarble;
            current.next = newMarble;
            current = newMarble;
        }
    }

    writeln(scores.maxElement);
}
