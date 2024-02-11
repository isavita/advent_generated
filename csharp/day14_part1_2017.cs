
using System;
using System.IO;
using System.Linq;
using System.Text;

class Program
{
    static void ReverseSection(int[] arr, int start, int length)
    {
        int n = arr.Length;
        for (int i = start, j = start + length - 1; i < j; i++, j--)
        {
            int temp = arr[i % n];
            arr[i % n] = arr[j % n];
            arr[j % n] = temp;
        }
    }

    static string KnotHash(string input)
    {
        var lengths = input.Select(c => (int)c).ToList();
        lengths.AddRange(new int[] { 17, 31, 73, 47, 23 });

        int[] list = Enumerable.Range(0, 256).ToArray();

        int position = 0;
        int skip = 0;
        for (int round = 0; round < 64; round++)
        {
            foreach (int length in lengths)
            {
                ReverseSection(list, position, length);
                position += length + skip;
                skip++;
            }
        }

        int[] denseHash = new int[16];
        for (int i = 0; i < 16; i++)
        {
            int xor = 0;
            for (int j = 0; j < 16; j++)
            {
                xor ^= list[i * 16 + j];
            }
            denseHash[i] = xor;
        }

        StringBuilder hexHash = new StringBuilder();
        foreach (int v in denseHash)
        {
            hexHash.Append(v.ToString("x2"));
        }

        return hexHash.ToString();
    }

    static string HexToBinary(string hexStr)
    {
        StringBuilder binaryStr = new StringBuilder();
        foreach (char hexDigit in hexStr)
        {
            int val = Convert.ToInt32(hexDigit.ToString(), 16);
            binaryStr.Append(Convert.ToString(val, 2).PadLeft(4, '0'));
        }
        return binaryStr.ToString();
    }

    static void Main()
    {
        string keyString = File.ReadAllText("input.txt").Trim();
        int totalUsed = 0;

        for (int i = 0; i < 128; i++)
        {
            string rowKey = $"{keyString}-{i}";
            string hash = KnotHash(rowKey);
            string binaryRow = HexToBinary(hash);

            totalUsed += binaryRow.Count(bit => bit == '1');
        }

        Console.WriteLine(totalUsed);
    }
}
