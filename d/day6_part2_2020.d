import std.stdio;
import std.file;
import std.array;

void main()
{
    string data = cast(string) read("input.txt");
    auto groups = data.split("\n\n");

    int partOneSum = 0;
    int partTwoSum = 0;

    foreach (group; groups)
    {
        auto lines = group.split("\n");

        int[] anyoneYes = new int[26];
        int[] everyoneYes = new int[26];

        foreach (line; lines)
        {
            foreach (char c; line)
            {
                anyoneYes[c - 'a'] = 1;
                everyoneYes[c - 'a']++;
            }
        }

        foreach (count; anyoneYes)
        {
            partOneSum += count;
        }

        foreach (count; everyoneYes)
        {
            if (count == cast(int) lines.length)
            {
                partTwoSum++;
            }
        }
    }

    writeln(partOneSum);
    writeln(partTwoSum);
}