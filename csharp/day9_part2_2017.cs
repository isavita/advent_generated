
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");

        int score = 0;
        int depth = 0;
        bool inGarbage = false;
        bool cancelNext = false;
        int garbageCount = 0;

        foreach (char ch in input)
        {
            if (cancelNext)
            {
                cancelNext = false;
                continue;
            }

            if (inGarbage)
            {
                if (ch == '!')
                {
                    cancelNext = true;
                }
                else if (ch == '>')
                {
                    inGarbage = false;
                }
                else
                {
                    garbageCount++;
                }
            }
            else
            {
                switch (ch)
                {
                    case '{':
                        depth++;
                        break;
                    case '}':
                        score += depth;
                        depth--;
                        break;
                    case '<':
                        inGarbage = true;
                        break;
                }
            }
        }

        Console.WriteLine(garbageCount);
    }
}
