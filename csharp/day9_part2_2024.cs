
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").TrimEnd('\n', '\r');
        int n = input.Length;
        int totalSize = 0;
        for (int i = 0; i < n; i++)
            if (input[i] >= '0' && input[i] <= '9')
                totalSize += input[i] - '0';

        if (totalSize == 0)
        {
            Console.WriteLine(0);
            return;
        }

        int fileCount = (n + 1) / 2;
        int[] disk = new int[totalSize];
        int[] start = new int[fileCount];
        int[] end = new int[fileCount];
        int pos = 0;
        int fid = 0;

        for (int i = 0; i < n; i++)
        {
            int len = input[i] - '0';
            if ((i & 1) == 0)
            {
                start[fid] = pos;
                end[fid] = pos + len - 1;
                for (int j = 0; j < len; j++) disk[pos + j] = fid;
                fid++;
            }
            else
            {
                for (int j = 0; j < len; j++) disk[pos + j] = -1;
            }
            pos += len;
        }

        for (int i = fileCount - 1; i >= 0; i--)
        {
            int len = end[i] - start[i] + 1;
            int best = -1, cur = 0;
            for (int j = 0; j < start[i]; j++)
            {
                if (disk[j] == -1)
                {
                    if (cur == 0) best = j;
                    cur++;
                    if (cur == len) break;
                }
                else
                {
                    cur = 0;
                    best = -1;
                }
            }
            if (best != -1 && cur == len)
            {
                for (int j = 0; j < len; j++) disk[best + j] = i;
                for (int j = start[i]; j <= end[i]; j++) disk[j] = -1;
            }
        }

        long checksum = 0;
        for (int i = 0; i < totalSize; i++)
            if (disk[i] != -1) checksum += (long)i * disk[i];

        Console.WriteLine(checksum);
    }
}
