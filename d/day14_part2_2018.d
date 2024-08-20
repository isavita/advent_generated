import std.stdio;
import std.file;
import std.conv;
import std.string; // Added import for std.string

void main()
{
    string input = cast(string)std.file.read("input.txt");
    int[] scores = [3, 7];
    int elf1 = 0;
    int elf2 = 1;
    string target = input.strip(); // Changed to strip() from std.string
    int targetLength = cast(int)target.length; // Casted to int

    while (true)
    {
        int sum = scores[elf1] + scores[elf2];
        if (sum >= 10)
        {
            scores ~= sum / 10;
            if (checkMatch(scores, target, targetLength))
            {
                writeln(scores.length - targetLength);
                break;
            }
        }
        scores ~= sum % 10;
        if (checkMatch(scores, target, targetLength))
        {
            writeln(scores.length - targetLength);
            break;
        }
        elf1 = cast(int)((elf1 + 1 + scores[elf1]) % scores.length); // Casted to int
        elf2 = cast(int)((elf2 + 1 + scores[elf2]) % scores.length); // Casted to int
    }
}

bool checkMatch(int[] scores, string target, int targetLength)
{
    if (scores.length < targetLength)
        return false;

    foreach_reverse (i, c; target)
    {
        if (scores[$ - targetLength + i] != to!int(c.to!string))
            return false;
    }
    return true;
}