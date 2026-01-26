
using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

public class JsonSum
{
    public static void Main(string[] args)
    {
        try
        {
            string input = File.ReadAllText("input.txt");
            Console.WriteLine("Part 1 Sum: " + SumNumbersPart1(input));
            Console.WriteLine("Part 2 Sum: " + SumNumbersPart2(input));
        }
        catch (FileNotFoundException e)
        {
            Console.Error.WriteLine("Error reading file: " + e.Message);
        }
    }

    static int SumNumbersPart1(string json)
    {
        int sum = 0;
        var matches = Regex.Matches(json, "-?\\d+");
        foreach (Match match in matches)
        {
            sum += int.Parse(match.Value);
        }
        return sum;
    }

    static int SumNumbersPart2(string json)
    {
        var parsedJson = ParseJson(json);
        return SumNumbersRecursive(parsedJson);
    }

    static int SumNumbersRecursive(object obj)
    {
        if (obj is int i)
        {
            return i;
        }
        else if (obj is List<object> list)
        {
            int sum = 0;
            foreach (var item in list)
            {
                sum += SumNumbersRecursive(item);
            }
            return sum;
        }
        else if (obj is Dictionary<string, object> dict)
        {
            if (dict.ContainsValue("red"))
            {
                return 0;
            }
            int sum = 0;
            foreach (var value in dict.Values)
            {
                sum += SumNumbersRecursive(value);
            }
            return sum;
        }
        return 0;
    }

    static object ParseJson(string json)
    {
        json = json.Trim();

        if (json.StartsWith("["))
        {
            var list = new List<object>();
            json = json.Substring(1, json.Length - 2).Trim();
            if (string.IsNullOrEmpty(json)) return list;

            int bracketCount = 0;
            int braceCount = 0;
            int start = 0;
            for (int i = 0; i < json.Length; i++)
            {
                char c = json[i];
                if (c == '[') bracketCount++;
                else if (c == ']') bracketCount--;
                else if (c == '{') braceCount++;
                else if (c == '}') braceCount--;
                else if (c == ',' && bracketCount == 0 && braceCount == 0)
                {
                    list.Add(ParseJson(json.Substring(start, i - start)));
                    start = i + 1;
                }
            }
            list.Add(ParseJson(json.Substring(start)));
            return list;
        }
        else if (json.StartsWith("{"))
        {
            var dict = new Dictionary<string, object>();
            json = json.Substring(1, json.Length - 2).Trim();
            if (string.IsNullOrEmpty(json)) return dict;

            int bracketCount = 0;
            int braceCount = 0;
            int start = 0;
            var pairs = new List<string>();

            for (int i = 0; i < json.Length; i++)
            {
                char c = json[i];
                if (c == '[') bracketCount++;
                else if (c == ']') bracketCount--;
                else if (c == '{') braceCount++;
                else if (c == '}') braceCount--;
                else if (c == ',' && bracketCount == 0 && braceCount == 0)
                {
                    pairs.Add(json.Substring(start, i - start));
                    start = i + 1;
                }
            }
            pairs.Add(json.Substring(start));

            foreach (var pairStr in pairs)
            {
                var parts = pairStr.Split(new[] { ':' }, 2);
                if (parts.Length == 2)
                {
                    var key = parts[0].Trim().Replace("\"", "");
                    var valueStr = parts[1].Trim();
                    dict[key] = ParseJson(valueStr);
                }
            }
            return dict;
        }
        else if (int.TryParse(json, out int number))
        {
            return number;
        }
        else
        {
            return json.Replace("\"", "");
        }
    }
}
