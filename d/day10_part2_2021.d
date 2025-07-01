
import std.stdio;
import std.algorithm;
import std.array;
import std.string;

void main() {
    static immutable char[char] pairings = [
        ')': '(', ']': '[', '}': '{', '>': '<'
    ];
    static immutable long[char] scoreValues = [
        '(': 1, '[': 2, '{': 3, '<': 4
    ];
    static immutable bool[char] isOpening = [
        '(': true, '[': true, '{': true, '<': true
    ];

    long[] scores;
    auto file = File("input.txt", "r");

    foreach (line; file.byLineCopy()) {
        char[] stack;
        bool corrupted = false;

        foreach (char c; line.strip()) {
            if (c in isOpening) {
                stack ~= c;
            } else {
                if (stack.length == 0 || stack[$ - 1] != pairings[c]) {
                    corrupted = true;
                    break;
                }
                stack.length--;
            }
        }

        if (!corrupted && stack.length > 0) {
            long currentScore = 0;
            foreach_reverse (char openChar; stack) {
                currentScore = currentScore * 5 + scoreValues[openChar];
            }
            scores ~= currentScore;
        }
    }

    scores.sort();
    writeln(scores[scores.length / 2]);
}
