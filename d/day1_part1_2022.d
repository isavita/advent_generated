import std.stdio;
import std.file;
import std.conv;

void main()
{
    int maxCalories = 0;
    int currentCalories = 0;

    auto file = File("input.txt", "r");
    foreach (line; file.byLine())
    {
        if (line == "")
        {
            if (currentCalories > maxCalories)
            {
                maxCalories = currentCalories;
            }
            currentCalories = 0;
            continue;
        }

        int calories = to!int(line);
        currentCalories += calories;
    }

    if (currentCalories > maxCalories)
    {
        maxCalories = currentCalories;
    }

    writeln(maxCalories);
}