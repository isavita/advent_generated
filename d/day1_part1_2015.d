
import std.file;
import std.stdio;

void main()
{
    string input = cast(string) read("input.txt");
    
    int floor = 0;
    
    foreach (char c; input)
    {
        if (c == '(')
        {
            floor++;
        }
        else if (c == ')')
        {
            floor--;
        }
    }
    
    writeln(floor);
}
