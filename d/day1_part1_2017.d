import std.file;
import std.stdio;
import std.string;

void main()
{
    string data = cast(string) readText("input.txt");
    data = data.strip();

    int sum = 0;

    foreach (i; 0 .. data.length)
    {
        int next = cast(int)((i + 1) % data.length);
        if (data[i] == data[next])
        {
            sum += (data[i] - '0');
        }
    }

    writeln(sum);
}