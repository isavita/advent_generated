
import std.stdio;
import std.ascii;

void main()
{
    int sum = 0;
    auto file = File("input.txt", "r");

    foreach(line; file.byLine())
    {
        if(line.length == 0)
            continue;

        int firstDigit = -1;
        int lastDigit = -1;

        foreach(r; line)
        {
            if(isDigit(r))
            {
                if(firstDigit == -1)
                    firstDigit = r - '0';
                lastDigit = r - '0';
            }
        }

        if(firstDigit != -1 && lastDigit != -1)
        {
            int value = (firstDigit * 10) + lastDigit;
            sum += value;
        }
    }

    writeln(sum);
}
