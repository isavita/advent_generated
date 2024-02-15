
import std.stdio;
import std.file;
import std.conv;

void main()
{
    long[] numbers; // Changed int[] to long[] to handle larger numbers
    foreach (line; File("input.txt").byLine())
    {
        numbers ~= to!long(line); // Changed to!int to to!long
    }

    long invalidNumber = 14360655;

    foreach (i, num; numbers)
    {
        long sum = num;
        long min = num;
        long max = num;
        foreach (j; i + 1 .. numbers.length)
        {
            sum += numbers[j];
            if (numbers[j] < min)
            {
                min = numbers[j];
            }
            if (numbers[j] > max)
            {
                max = numbers[j];
            }
            if (sum == invalidNumber)
            {
                writeln(min + max);
                return;
            }
            else if (sum > invalidNumber)
            {
                break;
            }
        }
    }
}
