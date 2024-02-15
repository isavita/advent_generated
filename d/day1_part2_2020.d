import std.stdio;
import std.file;
import std.conv;

void main()
{
    int[] expenses;
    foreach (line; File("input.txt").byLine())
    {
        expenses ~= to!int(line);
    }

    foreach (i, expense1; expenses)
    {
        foreach (j, expense2; expenses[i+1 .. $])
        {
            foreach (k, expense3; expenses[j+1 .. $])
            {
                if (expense1 + expense2 + expense3 == 2020)
                {
                    writeln(expense1 * expense2 * expense3);
                    return;
                }
            }
        }
    }
}