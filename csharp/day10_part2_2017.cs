
using System;
using System.IO;
using System.Linq;
using System.Text;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();

        int[] lengths = input.Select(c => (int)c).Concat(new int[] { 17, 31, 73, 47, 23 }).ToArray();

        int[] list = Enumerable.Range(0, 256).ToArray();
        int currentPosition = 0;
        int skipSize = 0;

        for (int round = 0; round < 64; round++)
        {
            foreach (int length in lengths)
            {
                for (int i = 0; i < length / 2; i++)
                {
                    int start = (currentPosition + i) % 256;
                    int end = (currentPosition + length - 1 - i) % 256;
                    int temp = list[start];
                    list[start] = list[end];
                    list[end] = temp;
                }
                currentPosition = (currentPosition + length + skipSize) % 256;
                skipSize++;
            }
        }

        byte[] denseHash = new byte[16];
        for (int i = 0; i < 256; i += 16)
        {
            int xor = 0;
            for (int j = 0; j < 16; j++)
            {
                xor ^= list[i + j];
            }
            denseHash[i / 16] = (byte)xor;
        }

        string hexHash = BitConverter.ToString(denseHash).Replace("-", "").ToLower();

        Console.WriteLine(hexHash);
    }
}
