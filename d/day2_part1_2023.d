import std.stdio;
import std.file;
import std.string;
import std.conv;

void main()
{
    string data = cast(string) read("input.txt");
    string[] games = data.strip().splitLines();
    
    int sum = 0;
    
    foreach (game; games)
    {
        string[] parts = game.split(":");
        int gameId = parts[0].strip().split(" ")[1].to!int;
        
        string[] sets = parts[1].split(";");
        
        bool possible = true;
        
        foreach (set; sets)
        {
            string[] cubes = set.split(",");
            
            foreach (cube; cubes)
            {
                string[] details = cube.strip().split(" ");
                int count = details[0].to!int;
                string color = details[1];
                
                if ((color == "red" && count > 12) || 
                    (color == "green" && count > 13) || 
                    (color == "blue" && count > 14))
                {
                    possible = false;
                    break;
                }
            }
            
            if (!possible)
            {
                break;
            }
        }
        
        if (possible)
        {
            sum += gameId;
        }
    }
    
    writeln(sum);
}