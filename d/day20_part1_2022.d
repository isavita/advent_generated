
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.array;
import std.algorithm;

struct Number {
    long val;
    size_t id;
}

void main() {
    auto initialNumbers = readText("input.txt")
        .strip
        .splitLines
        .map!(to!long)
        .array;

    size_t n = initialNumbers.length;
    if (n == 0) {
        writeln(0);
        return;
    }

    Number[] originalOrder;
    originalOrder.reserve(n);
    foreach(i, v; initialNumbers) {
        originalOrder ~= Number(v, i);
    }

    auto mixedList = originalOrder.dup;

    if (n > 1) {
        long modulus = n - 1;
        foreach(ref numToMove; originalOrder) {
            auto oldIndex = mixedList.countUntil!(a => a.id == numToMove.id);
            mixedList.remove(oldIndex);

            long newIndex = (oldIndex + numToMove.val);
            newIndex = (newIndex % modulus + modulus) % modulus;

            mixedList.insertInPlace(newIndex, numToMove);
        }
    }

    auto zeroIndex = mixedList.countUntil!(a => a.val == 0);
    long sum = 0;
    sum += mixedList[(zeroIndex + 1000) % n].val;
    sum += mixedList[(zeroIndex + 2000) % n].val;
    sum += mixedList[(zeroIndex + 3000) % n].val;

    writeln(sum);
}
