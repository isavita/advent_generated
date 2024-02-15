
import std.file;
import std.stdio;

void main()
{
    string input = cast(string) read("input.txt");
    
    int floor = 0;
    int position = 0;
    bool basementFound = false;
    
    foreach (char c; input)
    {
        position++;
        
        if (c == '(')
        {
            floor++;
        }
        else if (c == ')')
        {
            floor--;
        }
        
        if (floor == -1 && !basementFound)
        {
            writeln(position);
            basementFound = true;
        }
    }
}
