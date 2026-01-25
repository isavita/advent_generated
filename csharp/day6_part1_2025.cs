
using System;
using System.IO;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int maxw = 0;
        foreach (var line in lines)
        {
            maxw = Math.Max(maxw, line.Length);
        }

        string grandTotal = "0";
        bool inBlock = false;
        int startColumn = 0;
        for (int col = 0; col <= maxw; col++)
        {
            bool isSeparator = true;
            foreach (var line in lines)
            {
                if (col < line.Length && !char.IsWhiteSpace(line[col]))
                {
                    isSeparator = false;
                    break;
                }
            }

            if (!isSeparator)
            {
                if (!inBlock)
                {
                    inBlock = true;
                    startColumn = col;
                }
            }
            else
            {
                if (inBlock)
                {
                    grandTotal = ProcessBlock(lines, startColumn, col - 1, grandTotal);
                    inBlock = false;
                }
            }
        }

        Console.WriteLine($"Grand total: {grandTotal}");
    }

    private static string ProcessBlock(string[] lines, int startColumn, int endColumn, string grandTotal)
    {
        string[] numbers = new string[1000];
        int numberCount = 0;
        int operation = 0; // 0 - none, 1 - add, 2 - multiply

        for (int i = 0; i < lines.Length; i++)
        {
            if (startColumn >= lines[i].Length) continue;

            int end = Math.Min(endColumn + 1, lines[i].Length);
            string segment = lines[i].Substring(startColumn, end - startColumn).Trim();

            if (segment == "+") operation = 1;
            else if (segment == "*") operation = 2;
            else if (!string.IsNullOrEmpty(segment))
            {
                numbers[numberCount++] = segment;
            }
        }

        if (numberCount == 0) return grandTotal;

        string result;
        if (operation == 1)
        {
            result = AddNumbers(numbers, numberCount);
        }
        else if (operation == 2)
        {
            result = MultiplyNumbers(numbers, numberCount);
        }
        else if (numberCount == 1)
        {
            result = numbers[0];
        }
        else
        {
            return grandTotal;
        }

        return Add(grandTotal, result);
    }

    private static string AddNumbers(string[] numbers, int count)
    {
        string result = "0";
        for (int i = 0; i < count; i++)
        {
            result = Add(result, numbers[i]);
        }
        return result;
    }

    private static string MultiplyNumbers(string[] numbers, int count)
    {
        string result = "1";
        for (int i = 0; i < count; i++)
        {
            result = Multiply(result, numbers[i]);
        }
        return result;
    }

    private static string Add(string a, string b)
    {
        int maxLength = Math.Max(a.Length, b.Length);
        int[] result = new int[maxLength + 1];

        for (int i = 0; i < maxLength; i++)
        {
            int digitA = i < a.Length ? a[a.Length - 1 - i] - '0' : 0;
            int digitB = i < b.Length ? b[b.Length - 1 - i] - '0' : 0;
            result[i] += digitA + digitB;
            if (result[i] >= 10)
            {
                result[i + 1] += result[i] / 10;
                result[i] %= 10;
            }
        }

        int start = result.Length - 1;
        while (start > 0 && result[start] == 0) start--;

        char[] chars = new char[start + 1];
        for (int i = start; i >= 0; i--)
        {
            chars[start - i] = (char)(result[i] + '0');
        }

        return new string(chars);
    }

    private static string Multiply(string a, string b)
    {
        int[] result = new int[a.Length + b.Length];

        for (int i = a.Length - 1; i >= 0; i--)
        {
            for (int j = b.Length - 1; j >= 0; j--)
            {
                int digitA = a[i] - '0';
                int digitB = b[j] - '0';
                result[i + j + 1] += digitA * digitB;
            }
        }

        for (int i = result.Length - 1; i > 0; i--)
        {
            if (result[i] >= 10)
            {
                result[i - 1] += result[i] / 10;
                result[i] %= 10;
            }
        }

        int start = 0;
        while (start < result.Length && result[start] == 0) start++;

        char[] chars = new char[result.Length - start];
        for (int i = start; i < result.Length; i++)
        {
            chars[i - start] = (char)(result[i] + '0');
        }

        return new string(chars);
    }
}
