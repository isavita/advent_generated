
using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;

class Program
{
    static void Main()
    {
        string doorId = File.ReadAllText("input.txt").Trim();
        char[] password = new char[8];
        bool[] found = new bool[8];
        int filled = 0;
        ulong index = 0;
        StringBuilder sb = new StringBuilder(128);

        while (filled < 8)
        {
            sb.Clear().Append(doorId).Append(index);
            byte[] hash;
            using (MD5 md5 = MD5.Create())
                hash = md5.ComputeHash(Encoding.ASCII.GetBytes(sb.ToString()));

            if (hash[0] == 0 && hash[1] == 0 && (hash[2] & 0xF0) == 0)
            {
                int pos = hash[2] & 0x0F;
                if (pos < 8 && !found[pos])
                {
                    password[pos] = "0123456789abcdef"[hash[3] >> 4];
                    found[pos] = true;
                    filled++;
                }
            }
            index++;
        }
        Console.WriteLine(new string(password));
    }
}
