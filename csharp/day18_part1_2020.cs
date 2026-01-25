
using System;
using System.IO;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        if (!File.Exists("input.txt"))
        {
            Console.WriteLine("Error opening file");
            return;
        }

        long totalSum = 0;
        string[] lines = File.ReadAllLines("input.txt");

        foreach (string line in lines)
        {
            if (!string.IsNullOrWhiteSpace(line))
            {
                totalSum += Evaluate(line.Trim());
            }
        }

        Console.WriteLine(totalSum);
    }

    public static long Evaluate(string expression)
    {
        while (expression.Contains('(') || expression.Contains(')'))
        {
            int openParenIndex = expression.LastIndexOf('(');
            int closeParenIndex = expression.IndexOf(')', openParenIndex);

            if (closeParenIndex == -1)
            {
                throw new FormatException("Invalid expression");
            }

            long innerResult = EvaluateSimple(expression.Substring(openParenIndex + 1, closeParenIndex - openParenIndex - 1));
            expression = expression.Substring(0, openParenIndex) + innerResult + expression.Substring(closeParenIndex + 1);
        }

        return EvaluateSimple(expression);
    }

    public static long EvaluateSimple(string expr)
    {
        string[] parts = Regex.Split(expr.Trim(), @"\s+");

        long currentVal = long.Parse(parts[0]);

        for (int i = 1; i < parts.Length; i += 2)
        {
            char op = parts[i][0];
            long nextVal = long.Parse(parts[i + 1]);

            switch (op)
            {
                case '+':
                    currentVal += nextVal;
                    break;
                case '*':
                    currentVal *= nextVal;
                    break;
                default:
                    throw new FormatException("Invalid operator");
            }
        }

        return currentVal;
    }
}
