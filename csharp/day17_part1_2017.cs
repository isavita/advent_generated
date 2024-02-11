
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        int steps = int.Parse(input.Trim());
        int[] buffer = new int[] { 0 };
        int currentPos = 0;

        for (int i = 1; i <= 2017; i++)
        {
            currentPos = (currentPos + steps) % buffer.Length;
            Array.Resize(ref buffer, buffer.Length + 1);
            Array.Copy(buffer, currentPos, buffer, currentPos + 1, buffer.Length - currentPos - 1);
            buffer[currentPos + 1] = i;
            currentPos++;
        }

        for (int i = 0; i < buffer.Length; i++)
        {
            if (buffer[i] == 2017)
            {
                Console.WriteLine(buffer[(i + 1) % buffer.Length]);
                break;
            }
        }
    }
}
