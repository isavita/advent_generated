import std.file;
import std.conv;
import std.stdio;

void main()
{
    string input = cast(string) read("input.txt");

    int recipes = to!int(input);

    int[] scoreboard = [3, 7];
    int elf1 = 0;
    int elf2 = 1;

    while(scoreboard.length < recipes + 10)
    {
        int sum = scoreboard[elf1] + scoreboard[elf2];
        if(sum >= 10)
        {
            scoreboard ~= sum / 10;
            scoreboard ~= sum % 10;
        }
        else
        {
            scoreboard ~= sum;
        }

        elf1 = (elf1 + 1 + scoreboard[elf1]) % cast(int)scoreboard.length;
        elf2 = (elf2 + 1 + scoreboard[elf2]) % cast(int)scoreboard.length;
    }

    string result = "";
    foreach(i; 0..10)
    {
        result ~= to!string(scoreboard[recipes + i]);
    }

    writeln(result);
}