import std.stdio;
import std.file;
import std.conv;

void main()
{
    string input = cast(string) read("input.txt");
    int sum = 0;
    int i = 0;
    while (i < input.length)
    {
        if (input[i] == '-' || (input[i] >= '0' && input[i] <= '9'))
        {
            int j = i;
            while (j < input.length && (input[j] == '-' || (input[j] >= '0' && input[j] <= '9')))
            {
                j++;
            }
            int num = to!int(input[i .. j]);
            sum += num;
            i = j;
        }
        else
        {
            i++;
        }
    }
    writeln(sum);
}