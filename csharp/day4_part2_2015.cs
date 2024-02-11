
using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;

class Program
{
    static void Main()
    {
        string secretKey = File.ReadAllText("input.txt").Trim();
        int number = 0;

        using (MD5 md5 = MD5.Create())
        {
            while (true)
            {
                string input = secretKey + number;
                byte[] inputBytes = Encoding.ASCII.GetBytes(input);
                byte[] hashBytes = md5.ComputeHash(inputBytes);

                if (IsLeadingZeros(hashBytes, 6)) // Checking for six leading zeroes
                {
                    Console.WriteLine(number);
                    break;
                }

                number++;
            }
        }
    }

    static bool IsLeadingZeros(byte[] hashBytes, int leadingZeros)
    {
        for (int i = 0; i < leadingZeros / 2; i++)
        {
            if (hashBytes[i] != 0)
            {
                return false;
            }
        }

        if (leadingZeros % 2 == 1 && (hashBytes[leadingZeros / 2] & 0xF0) != 0)
        {
            return false;
        }

        return true;
    }
}
