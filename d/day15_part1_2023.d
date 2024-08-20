import std.stdio;
import std.file;
import std.conv;
import std.array;

void main()
{
    string data = cast(string) read("input.txt");
    string[] steps = data.split(",");

    int sum = 0;
    foreach (step; steps)
    {
        int hashValue = 0;
        foreach (char c; step)
        {
            int asciiCode = cast(int)c;
            hashValue += asciiCode;
            hashValue *= 17;
            hashValue %= 256;
        }
        sum += hashValue;
    }

    writeln(sum);
}