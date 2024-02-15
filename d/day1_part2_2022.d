
import std.stdio;
import std.algorithm;
import std.array;
import std.conv;

void main()
{
    int[] caloriesList;
    int currentCalories = 0;

    foreach (line; File("input.txt").byLine())
    {
        if (line.length == 0)
        {
            caloriesList ~= currentCalories;
            currentCalories = 0;
            continue;
        }

        currentCalories += to!int(line);
    }

    caloriesList ~= currentCalories;
    caloriesList.sort!"a > b"; // Sort in descending order

    int topThreeSum = caloriesList[0..min(3, caloriesList.length)].sum;

    writeln(topThreeSum);
}
