
import std.file;
import std.conv;
import std.array;
import std.stdio;

void main()
{
    string input = cast(string)std.file.read("input.txt");
    int start = input.split("-")[0].to!int;
    int end = input.split("-")[1].to!int;

    int count = 0;
    foreach (i; start .. end)
    {
        string password = to!string(i);
        bool hasAdjacent = false;
        bool neverDecrease = true;

        for (int j = 0; j < password.length - 1; j++)
        {
            if (password[j] == password[j + 1])
            {
                hasAdjacent = true;
            }
            if (password[j] > password[j + 1])
            {
                neverDecrease = false;
                break;
            }
        }

        if (hasAdjacent && neverDecrease)
        {
            count++;
        }
    }

    writeln(count);
}
