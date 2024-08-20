import std.stdio;
import std.file;

void main()
{
    auto file = File("input.txt", "r");
    if (!file.isOpen)
    {
        writeln("Failed to open file");
        return;
    }

    int x = 1, y = 1;
    foreach (line; file.byLine())
    {
        foreach (char c; line)
        {
            switch (c)
            {
                case 'U':
                    if (y > 0) y--;
                    break;
                case 'D':
                    if (y < 2) y++;
                    break;
                case 'L':
                    if (x > 0) x--;
                    break;
                case 'R':
                    if (x < 2) x++;
                    break;
                default:
                    break;
            }
        }
        write(getButton(x, y));
    }
    writeln();
}

char getButton(int x, int y)
{
    return cast(char)(x + 1 + y * 3 + '0');
}