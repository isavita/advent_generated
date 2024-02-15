
import std.stdio;
import std.file;
import std.conv;

struct LLNode {
    int elfNum;
    int presents;
    LLNode* next;
}

int elephant(string input) {
    int startingElves = input.to!int;
    LLNode* root = new LLNode(1, 1, null);
    LLNode* iter = root;
    foreach (i; 2..startingElves + 1) {
        iter.next = new LLNode(i, 1, null);
        iter = iter.next;
    }
    iter.next = root;

    bool isOddLength = startingElves % 2 == 1;
    LLNode* beforeAcross = root;
    foreach (i; 0..startingElves / 2 - 1) {
        beforeAcross = beforeAcross.next;
    }

    while (root.next != root) {
        root.presents += beforeAcross.next.presents;
        beforeAcross.next = beforeAcross.next.next;

        if (isOddLength) {
            beforeAcross = beforeAcross.next;
        }
        isOddLength = !isOddLength;
        root = root.next;
    }

    return root.elfNum;
}

void main() {
    string input = cast(string)std.file.read("input.txt");
    writeln(elephant(input));
}
