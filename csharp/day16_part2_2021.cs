
using System;
using System.IO;
using System.Linq;

public class Program
{
    public static void Main()
    {
        var hexStr = File.ReadAllText("input.txt").Trim();
        var binStr = HexToBin(hexStr);
        var finalResult = ParsePacket(binStr, 0);
        Console.WriteLine(finalResult.value);
    }

    private static string HexToBin(string hex)
    {
        return string.Concat(hex.Select(c => Convert.ToString(Convert.ToInt32(c.ToString(), 16), 2).PadLeft(4, '0')));
    }

    private static (int version, int idx, long value) ParsePacket(string binStr, int idx)
    {
        var version = Convert.ToInt32(binStr.Substring(idx, 3), 2);
        var typeID = Convert.ToInt32(binStr.Substring(idx + 3, 3), 2);
        idx += 6;

        if (typeID == 4)
        {
            long value = 0;
            do
            {
                value = (value << 4) | Convert.ToInt64(binStr.Substring(idx + 1, 4), 2);
                idx += 5;
            } while (binStr[idx - 5] == '1');
            return (version, idx, value);
        }

        var lengthTypeID = binStr[idx];
        idx++;

        int subPacketLength = 0;
        int numSubPackets = 0;

        if (lengthTypeID == '0')
        {
            subPacketLength = Convert.ToInt32(binStr.Substring(idx, 15), 2);
            idx += 15;
        }
        else
        {
            numSubPackets = Convert.ToInt32(binStr.Substring(idx, 11), 2);
            idx += 11;
        }

        var values = new long[1024];
        var valueCount = 0;

        while ((lengthTypeID == '0' && subPacketLength > 0) || (lengthTypeID == '1' && numSubPackets > 0))
        {
            var subResult = ParsePacket(binStr, idx);
            values[valueCount++] = subResult.value;
            if (lengthTypeID == '0')
            {
                subPacketLength -= subResult.idx - idx;
            }
            else
            {
                numSubPackets--;
            }
            idx = subResult.idx;
        }

        long res = 0;
        switch (typeID)
        {
            case 0:
                res = values.Take(valueCount).Sum();
                break;
            case 1:
                res = 1;
                foreach (var val in values.Take(valueCount))
                {
                    res *= val;
                }
                break;
            case 2:
                res = values.Take(valueCount).Min();
                break;
            case 3:
                res = values.Take(valueCount).Max();
                break;
            case 5:
                res = values[0] > values[1] ? 1 : 0;
                break;
            case 6:
                res = values[0] < values[1] ? 1 : 0;
                break;
            case 7:
                res = values[0] == values[1] ? 1 : 0;
                break;
            default:
                throw new Exception($"Unknown typeID: {typeID}");
        }

        return (version, idx, res);
    }
}
