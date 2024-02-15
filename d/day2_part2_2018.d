
import std.stdio;
import std.file;

void main()
{
    auto file = File("input.txt", "r");
    string[] lines;
    foreach(line; file.byLine())
    {
        lines ~= line.idup;
    }
    file.close();

    foreach(i; 0 .. lines.length - 1)
    {
        foreach(j; i + 1 .. lines.length)
        {
            int diff = 0;
            foreach(k; 0 .. lines[i].length)
            {
                if(lines[i][k] != lines[j][k])
                {
                    diff++;
                    if(diff > 1)
                    {
                        break;
                    }
                }
            }
            if(diff == 1)
            {
                string common = "";
                foreach(k; 0 .. lines[i].length)
                {
                    if(lines[i][k] == lines[j][k])
                    {
                        common ~= lines[i][k];
                    }
                }
                writeln(common);
                return;
            }
        }
    }
}
