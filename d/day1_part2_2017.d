import std.stdio;
import std.string;
import std.file;

void main()
{
    string data = cast(string) readText("input.txt");
    data = data.strip();

    size_t halfway = data.length / 2;
    int sum = 0;

    foreach (i; 0 .. data.length)
    {
        size_t next = (i + halfway) % data.length;
        if (data[i] == data[next])
        {
            sum += cast(int)(data[i] - '0');
        }
    }

    writeln(sum);
}