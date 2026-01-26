
using System;
using System.IO;
using System.Text;

public class Lobby
{
    public static void Main(string[] args)
    {
        string fileName = "input.txt";
        long totalOutputJoltage = 0;

        try
        {
            string[] lines = File.ReadAllLines(fileName);
            foreach (string line in lines)
            {
                string trimmedLine = line.Trim();
                if (string.IsNullOrEmpty(trimmedLine))
                {
                    continue;
                }

                string largestSubsequence = FindLargestSubsequence(trimmedLine, 12);
                totalOutputJoltage += long.Parse(largestSubsequence);
            }

            Console.WriteLine(totalOutputJoltage);
        }
        catch (FileNotFoundException e)
        {
            Console.Error.WriteLine($"Error reading file: {e.Message}");
        }
        catch (FormatException e)
        {
            Console.Error.WriteLine($"Error processing numeric data: {e.Message}");
        }
    }

    private static string FindLargestSubsequence(string s, int k)
    {
        int n = s.Length;
        int toRemove = n - k;
        char[] stack = new char[n];
        int top = -1;

        for (int i = 0; i < n; i++)
        {
            char currentDigit = s[i];
            while (toRemove > 0 && top >= 0 && stack[top] < currentDigit)
            {
                top--;
                toRemove--;
            }
            stack[++top] = currentDigit;
        }

        return new string(stack, 0, k);
    }
}
