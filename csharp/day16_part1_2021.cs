
using System;
using System.IO;
using System.Text;

class Program
{
    static string HexToBin(string hex)
    {
        StringBuilder bin = new StringBuilder();
        foreach (char h in hex)
        {
            int b = Convert.ToInt32(h.ToString(), 16);
            bin.Append(Convert.ToString(b, 2).PadLeft(4, '0'));
        }
        return bin.ToString();
    }

    static Tuple<int, int> ParsePacket(string binStr, int idx)
    {
        int version = ((binStr[idx] - '0') << 2) | ((binStr[idx + 1] - '0') << 1) | (binStr[idx + 2] - '0');
        int typeID = ((binStr[idx + 3] - '0') << 2) | ((binStr[idx + 4] - '0') << 1) | (binStr[idx + 5] - '0');
        idx += 6;

        if (typeID == 4)
        {
            while (binStr[idx] == '1')
            {
                idx += 5;
            }
            idx += 5;
            return Tuple.Create(version, idx);
        }

        int lengthTypeID = binStr[idx] - '0';
        idx++;
        int numSubPackets = 0;
        int subPacketLength = 0;

        if (lengthTypeID == 0)
        {
            subPacketLength = 0;
            for (int i = 0; i < 15; i++)
            {
                subPacketLength = (subPacketLength << 1) | (binStr[idx] - '0');
                idx++;
            }
        }
        else
        {
            numSubPackets = 0;
            for (int i = 0; i < 11; i++)
            {
                numSubPackets = (numSubPackets << 1) | (binStr[idx] - '0');
                idx++;
            }
        }

        int versionSum = version;
        while (true)
        {
            if ((lengthTypeID == 0 && subPacketLength == 0) || (lengthTypeID == 1 && numSubPackets == 0))
            {
                break;
            }
            Tuple<int, int> result = ParsePacket(binStr, idx);
            int subVersion = result.Item1;
            int newIndex = result.Item2;
            versionSum += subVersion;

            if (lengthTypeID == 0)
            {
                subPacketLength -= newIndex - idx;
            }
            else
            {
                numSubPackets--;
            }
            idx = newIndex;
        }
        return Tuple.Create(versionSum, idx);
    }

    static void Main()
    {
        string hexStr = File.ReadAllText("input.txt").Trim();
        string binStr = HexToBin(hexStr);
        Tuple<int, int> result = ParsePacket(binStr, 0);
        Console.WriteLine(result.Item1);
    }
}
