
import std.file;
import std.stdio;

void main()
{
    string input = cast(string) read("input.txt");
    
    int x = 0;
    int y = 0;
    int[][] visited;
    
    visited ~= [0, 0];
    
    foreach (char direction; input)
    {
        if (direction == '^')
            y++;
        else if (direction == 'v')
            y--;
        else if (direction == '>')
            x++;
        else if (direction == '<')
            x--;
        
        bool found = false;
        foreach (coord; visited)
        {
            if (coord[0] == x && coord[1] == y)
            {
                found = true;
                break;
            }
        }
        
        if (!found)
            visited ~= [x, y];
    }
    
    writeln(visited.length);
}
