
import std.file;
import std.stdio;
import std.string;

void main()
{
    auto input = cast(string) read("input.txt");
    
    int niceCount = 0;
    foreach (line; input.splitLines())
    {
        if (isNicePart1(line))
            niceCount++;
    }
    
    writeln(niceCount);
}

bool isNicePart1(string str)
{
    int vowelCount = 0;
    bool hasDoubleLetter = false;
    bool hasDisallowedSubstring = false;
    
    char prevChar = '\0';
    char prevPrevChar = '\0';
    
    foreach (char c; str)
    {
        if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u')
            vowelCount++;
        
        if (c == prevChar)
            hasDoubleLetter = true;
        
        if ((prevPrevChar == 'a' && prevChar == 'b') ||
            (prevPrevChar == 'c' && prevChar == 'd') ||
            (prevPrevChar == 'p' && prevChar == 'q') ||
            (prevPrevChar == 'x' && prevChar == 'y'))
        {
            hasDisallowedSubstring = true;
        }
        
        prevPrevChar = prevChar;
        prevChar = c;
    }
    
    return vowelCount >= 3 && hasDoubleLetter && !hasDisallowedSubstring;
}
