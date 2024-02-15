
import std.file;
import std.array;
import std.conv;
import std.algorithm;
import std.stdio; // Added import statement for writeln

void main()
{
    auto input = cast(string) read("input.txt");
    auto ips = input.split("\n");

    int count = 0;
    foreach(ip; ips)
    {
        bool supportsTLS = false;
        bool inHypernet = false;
        bool abbaFound = false;

        foreach(i, c; ip)
        {
            if(c == '[')
            {
                inHypernet = true;
            }
            else if(c == ']')
            {
                inHypernet = false;
            }
            else if(i >= 3 && c == ip[i - 3] && c != ip[i - 2] && ip[i - 1] == ip[i - 2] && ip[i - 1] != c)
            {
                if(inHypernet)
                {
                    supportsTLS = false;
                    break;
                }
                abbaFound = true;
            }
        }

        if(abbaFound && !inHypernet)
        {
            supportsTLS = true;
        }

        if(supportsTLS)
        {
            count++;
        }
    }

    writeln(count);
}
