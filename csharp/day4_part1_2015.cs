
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

        while (true)
        {
            string input = secretKey + number;
            byte[] inputBytes = Encoding.ASCII.GetBytes(input);
            byte[] hashBytes = MD5.Create().ComputeHash(inputBytes);
            string hash = BitConverter.ToString(hashBytes).Replace("-", "").ToLower();

            if (hash.StartsWith("00000"))
            {
                Console.WriteLine(number);
                break;
            }

            number++;
        }
    }
}
