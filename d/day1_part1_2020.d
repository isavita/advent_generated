
import std.stdio;
import std.file;
import std.conv;

void main()
{
    auto file = File("input.txt", "r");
    int[] numbers;

    foreach(line; file.byLine())
    {
        if(line.length == 0)
            continue;

        numbers ~= to!int(line);
    }

    foreach(i; 0 .. numbers.length - 1)
    {
        foreach(j; i + 1 .. numbers.length)
        {
            if(numbers[i] + numbers[j] == 2020)
            {
                writeln(numbers[i] * numbers[j]);
                return;
            }
        }
    }
}
