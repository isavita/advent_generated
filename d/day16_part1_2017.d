import std.stdio;
import std.file;
import std.array;
import std.conv;
import std.string;

void main()
{
    string programs = "abcdefghijklmnop";
    string input = cast(string) read("input.txt");
    string[] moves = input.split(",");

    foreach (move; moves)
    {
        switch (move[0])
        {
            case 's':
                int x = move[1..$].to!int;
                programs = programs[$-x..$] ~ programs[0..$-x];
                break;
            case 'x':
                auto parts = move[1..$].split("/");
                int a = parts[0].to!int;
                int b = parts[1].to!int;
                programs = swap(programs, a, b);
                break;
            case 'p':
                auto parts = move[1..$].split("/");
                char a = parts[0][0];
                char b = parts[1][0];
                int ai = cast(int) programs.indexOf(a);
                int bi = cast(int) programs.indexOf(b);
                programs = swap(programs, ai, bi);
                break;
            default:
                break;
        }
    }

    writeln(programs);
}

string swap(string s, int a, int b)
{
    char[] chars = s.dup;
    char temp = chars[a];
    chars[a] = chars[b];
    chars[b] = temp;
    return cast(string) chars;
}