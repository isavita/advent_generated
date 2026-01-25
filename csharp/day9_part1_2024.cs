
using System;
using System.IO;

class Program
{
    static void Main()
    {
        const string INPUT_FILE = "input.txt";
        const int FREE_SPACE = -1;

        string content = File.ReadAllText(INPUT_FILE);
        if (content.Length == 0)
        {
            Console.WriteLine("0");
            return;
        }

        int totalSize = 0;
        foreach (char c in content)
            if (c >= '0' && c <= '9')
                totalSize += c - '0';

        if (totalSize == 0)
        {
            Console.WriteLine("0");
            return;
        }

        int[] disk = new int[totalSize];
        int pos = 0, digitIdx = 0;

        foreach (char c in content)
        {
            if (c >= '0' && c <= '9')
            {
                int len = c - '0';
                if ((digitIdx & 1) == 0)
                {
                    int fileId = digitIdx >> 1;
                    for (int i = 0; i < len; i++)
                        disk[pos++] = fileId;
                }
                else
                {
                    for (int i = 0; i < len; i++)
                        disk[pos++] = FREE_SPACE;
                }
                digitIdx++;
            }
        }

        int left = 0, right = totalSize - 1;
        while (left < right)
        {
            while (left < right && disk[left] != FREE_SPACE) left++;
            while (left < right && disk[right] == FREE_SPACE) right--;
            if (left < right)
            {
                disk[left] = disk[right];
                disk[right] = FREE_SPACE;
                left++;
                right--;
            }
        }

        ulong checksum = 0;
        for (int i = 0; i < totalSize; i++)
            if (disk[i] != FREE_SPACE)
                checksum += (ulong)i * (ulong)disk[i];

        Console.WriteLine(checksum);
    }
}
