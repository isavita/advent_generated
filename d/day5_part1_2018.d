
import std.file;
import std.array;
import std.algorithm;
import std.ascii; // Added import for toLower function
import std.stdio; // Added import for writeln function

void main()
{
    string polymer = cast(string) read("input.txt");
    
    while (true)
    {
        bool reacted = false;
        for (size_t i = 0; i < polymer.length - 1; i++)
        {
            if (polymer[i] != polymer[i + 1] && toLower(polymer[i]) == toLower(polymer[i + 1]))
            {
                polymer = polymer[0 .. i] ~ polymer[i + 2 .. $];
                reacted = true;
                break;
            }
        }
        if (!reacted)
        {
            break;
        }
    }
    
    writeln(polymer.length);
}
