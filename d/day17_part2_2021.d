import std.stdio;
import std.file;
import std.conv;
import std.string;

void main()
{
    string data = cast(string) read("input.txt");
    string[] parts = data.split(", ");
    
    string[] xParts = parts[0].split("..");
    int xMin = xParts[0].split("=")[1].to!int;
    int xMax = xParts[1].to!int;
    
    string[] yParts = parts[1].split("..");
    int yMin = yParts[0].split("=")[1].to!int;
    int yMax = yParts[1].to!int;
    
    int count = 0;
    
    foreach (int vxStart; 1 .. xMax + 1)
    {
        foreach (int vyStart; yMin .. -yMin)
        {
            int x = 0;
            int y = 0;
            int vx = vxStart;
            int vy = vyStart;
            
            while (x <= xMax && y >= yMin)
            {
                if (x >= xMin && x <= xMax && y >= yMin && y <= yMax)
                {
                    count++;
                    break;
                }
                
                x += vx;
                y += vy;
                
                if (vx > 0)
                {
                    vx--;
                }
                else if (vx < 0)
                {
                    vx++;
                }
                
                vy--;
            }
        }
    }
    
    writeln(count);
}