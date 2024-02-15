import std.stdio;
import std.file;
import std.conv;

void main()
{
    string input = cast(string) read("input.txt");
    foreach (_; 0 .. 50)
    {
        input = lookAndSay(input);
    }
    writeln(input.length);
}

string lookAndSay(string input)
{
    string result = "";
    int count = 1;
    char digit = input[0];
    
    foreach (i, c; input[1 .. $])
    {
        if (c == digit)
        {
            count++;
        }
        else
        {
            result ~= count.to!string ~ digit;
            count = 1;
            digit = c;
        }
    }
    
    result ~= count.to!string ~ digit;
    
    return result;
}