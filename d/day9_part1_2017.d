
import std.stdio;
import std.file;

void main()
{
    string input = cast(string) read("input.txt");
    
    int score = 0;
    int level = 0;
    bool inGarbage = false;
    bool ignoreNext = false;
    
    foreach (char c; input)
    {
        if (ignoreNext)
        {
            ignoreNext = false;
            continue;
        }
        
        if (inGarbage)
        {
            if (c == '!')
            {
                ignoreNext = true;
            }
            else if (c == '>')
            {
                inGarbage = false;
            }
        }
        else
        {
            if (c == '{')
            {
                level++;
            }
            else if (c == '}')
            {
                score += level;
                level--;
            }
            else if (c == '<')
            {
                inGarbage = true;
            }
        }
    }
    
    writeln(score);
}
