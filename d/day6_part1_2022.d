import std.stdio;
import std.file;

void main()
{
    string data = cast(string) read("input.txt");
    
    foreach (i; 0 .. data.length - 3)
    {
        if (data[i] != data[i+1] && data[i] != data[i+2] && data[i] != data[i+3] &&
            data[i+1] != data[i+2] && data[i+1] != data[i+3] && data[i+2] != data[i+3])
        {
            writeln(i + 4);
            break;
        }
    }
}