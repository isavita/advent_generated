import std.stdio;
import std.file;

void main()
{
    string data = cast(string) read("input.txt");
    
    writeln(findMarker(data, 4));
    writeln(findMarker(data, 14));
}

int findMarker(string data, int length)
{
    foreach (i; 0 .. cast(int)(data.length - length + 1))
    {
        bool unique = true;
        foreach (j; 0 .. length)
        {
            foreach (k; j + 1 .. length)
            {
                if (data[i + j] == data[i + k])
                {
                    unique = false;
                    break;
                }
            }
            if (!unique)
                break;
        }
        if (unique)
            return i + length;
    }
    return -1;
}