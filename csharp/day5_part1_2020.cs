
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        int maxSeatID = 0;
        foreach (string pass in lines)
        {
            string binaryPass = pass.Replace('F', '0').Replace('B', '1').Replace('L', '0').Replace('R', '1');
            int seatID = Decode(binaryPass);
            if (seatID > maxSeatID)
            {
                maxSeatID = seatID;
            }
        }

        Console.WriteLine(maxSeatID);
    }

    static int Decode(string pass)
    {
        int row = BinaryToInt(pass.Substring(0, 7));
        int column = BinaryToInt(pass.Substring(7));
        return row * 8 + column;
    }

    static int BinaryToInt(string binaryStr)
    {
        int result = 0;
        for (int i = 0; i < binaryStr.Length; i++)
        {
            if (binaryStr[i] == '1')
            {
                result |= 1 << (binaryStr.Length - i - 1);
            }
        }
        return result;
    }
}
