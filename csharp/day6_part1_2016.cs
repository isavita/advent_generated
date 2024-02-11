
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

class Program
{
    static void Main()
    {
        string[] messages = ReadMessagesFromFile("input.txt");
        if (messages != null)
        {
            string correctedMessage = GetCorrectedMessage(messages);
            Console.WriteLine(correctedMessage);
        }
        else
        {
            Console.WriteLine("Error reading file.");
        }
    }

    static string[] ReadMessagesFromFile(string fileName)
    {
        if (!File.Exists(fileName))
        {
            Console.WriteLine("File not found: " + fileName);
            return null;
        }

        return File.ReadAllLines(fileName);
    }

    static string GetCorrectedMessage(string[] messages)
    {
        if (messages.Length == 0)
        {
            return "";
        }

        int messageLength = messages[0].Length;
        var count = new Dictionary<char, int>[messageLength];
        for (int i = 0; i < count.Length; i++)
        {
            count[i] = new Dictionary<char, int>();
        }

        foreach (var message in messages)
        {
            for (int j = 0; j < message.Length; j++)
            {
                char character = message[j];
                if (!count[j].ContainsKey(character))
                {
                    count[j][character] = 0;
                }
                count[j][character]++;
            }
        }

        StringBuilder correctedMessage = new StringBuilder();
        foreach (var charCount in count)
        {
            correctedMessage.Append(GetMostCommonChar(charCount));
        }

        return correctedMessage.ToString();
    }

    static char GetMostCommonChar(Dictionary<char, int> count)
    {
        char maxChar = '\0';
        int maxCount = 0;
        foreach (var pair in count)
        {
            if (pair.Value > maxCount)
            {
                maxCount = pair.Value;
                maxChar = pair.Key;
            }
        }
        return maxChar;
    }
}
