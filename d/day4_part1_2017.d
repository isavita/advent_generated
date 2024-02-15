
import std.stdio;
import std.file;
import std.array;
import std.string;
import std.conv;

void main()
{
    string data = cast(string)read("input.txt");
    string[] passphrases = data.splitLines();
    int validCount = 0;

    foreach (passphrase; passphrases)
    {
        string[] words = passphrase.split();
        string[string] wordSet;

        bool valid = true;
        foreach (word; words)
        {
            if (word in wordSet)
            {
                valid = false;
                break;
            }
            wordSet[word] = "";
        }

        if (valid)
        {
            validCount++;
        }
    }

    writeln(validCount);
}
