import std.stdio;
import std.file;
import std.conv;
import std.string;

void main()
{
    string data = cast(string) read("input.txt");
    string[] lines = data.strip().split("\n");
    int sum = 0;

    foreach (line; lines)
    {
        string[] numsStr = line.split();
        int[] nums;

        foreach (numStr; numsStr)
        {
            int num = to!int(numStr);
            nums ~= num;
        }

        foreach (i, num1; nums)
        {
            foreach (j, num2; nums)
            {
                if (i != j && num1 % num2 == 0)
                {
                    sum += num1 / num2;
                }
            }
        }
    }

    writeln(sum);
}