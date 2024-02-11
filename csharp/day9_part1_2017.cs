
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Step 1: Read Input
        string input = File.ReadAllText("input.txt");

        // Step 2: Initialize Variables
        int score = 0;
        int depth = 0;
        bool inGarbage = false;
        bool cancelNext = false;

        // Step 3: Process Stream
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

        // Step 4: Print Score
        Console.WriteLine(score);
    }
}
