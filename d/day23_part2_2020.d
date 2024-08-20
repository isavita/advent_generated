import std.stdio;
import std.file;
import std.conv;

void main()
{
    string input = cast(string) read("input.txt");
    int[] cups;
    foreach (char c; input)
    {
        cups ~= c.to!int - '0'.to!int;
    }
    foreach (i; cast(int)cups.length + 1 .. 1000001)
    {
        cups ~= i;
    }

    int currentCup = cups[0];
    int[int] nextCup;
    foreach (i; 0 .. cast(int)cups.length - 1)
    {
        nextCup[cups[i]] = cups[i + 1];
    }
    nextCup[cups[$ - 1]] = cups[0];

    foreach (_; 0 .. 10000000)
    {
        int pick1 = nextCup[currentCup];
        int pick2 = nextCup[pick1];
        int pick3 = nextCup[pick2];

        int dest = currentCup - 1;
        if (dest == 0)
        {
            dest = 1000000;
        }
        while (dest == pick1 || dest == pick2 || dest == pick3)
        {
            dest--;
            if (dest == 0)
            {
                dest = 1000000;
            }
        }

        nextCup[currentCup] = nextCup[pick3];
        nextCup[pick3] = nextCup[dest];
        nextCup[dest] = pick1;

        currentCup = nextCup[currentCup];
    }

    int cup1 = nextCup[1];
    int cup2 = nextCup[cup1];
    writeln(cast(long)cup1 * cup2);
}