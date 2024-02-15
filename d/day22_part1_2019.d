
import std.algorithm;
import std.array;
import std.conv;
import std.file;
import std.range;
import std.string;
import std.stdio; // Added import for writeln

void main()
{
    auto instructions = readText("input.txt").strip().splitter("\n").array;

    int deckSize = 10007;
    int cardPosition = 2019;

    foreach (instruction; instructions)
    {
        if (instruction.startsWith("deal into new stack"))
        {
            cardPosition = deckSize - 1 - cardPosition;
        }
        else if (instruction.startsWith("cut"))
        {
            int n = instruction.split(" ").back.to!int;
            cardPosition = (cardPosition - n + deckSize) % deckSize;
        }
        else if (instruction.startsWith("deal with increment"))
        {
            int n = instruction.split(" ").back.to!int;
            cardPosition = (cardPosition * n) % deckSize;
        }
    }

    writeln(cardPosition);
}
