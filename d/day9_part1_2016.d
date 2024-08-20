import std.stdio;
import std.file;
import std.conv;

void main()
{
    string input = cast(string) read("input.txt");
    writeln(decompressedLength(input));
}

size_t decompressedLength(string data)
{
    size_t length = 0;
    for (size_t i = 0; i < data.length; i++)
    {
        if (data[i] == '(')
        {
            size_t j = i + 1;
            while (data[j] != 'x')
                j++;
            size_t k = j + 1;
            while (data[k] != ')')
                k++;
            
            size_t numChars = to!size_t(data[i+1 .. j]);
            size_t repeat = to!size_t(data[j+1 .. k]);
            
            length += numChars * repeat;
            i = k + numChars;
        }
        else if (data[i] != ' ' && data[i] != '\n')
        {
            length++;
        }
    }
    return length;
}