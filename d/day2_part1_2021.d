import std.stdio;
import std.file;
import std.conv;
import std.string;

void main()
{
    int horizontal = 0;
    int depth = 0;

    auto file = File("input.txt", "r");
    foreach (line; file.byLine())
    {
        auto parts = line.split(" ");
        auto command = parts[0];
        auto value = to!int(parts[1]);

        if (command == "forward")
        {
            horizontal += value;
        }
        else if (command == "down")
        {
            depth += value;
        }
        else if (command == "up")
        {
            depth -= value;
        }
    }

    writeln(horizontal * depth);
}