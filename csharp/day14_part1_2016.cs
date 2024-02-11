
using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;

class Program
{
    static void Main()
    {
        string salt = File.ReadAllText("input.txt").Trim();
        int keys = 0;
        int index = 0;
        while (keys < 64)
        {
            string hash = GetMD5Hash(salt + index);
            string triplet = FindTriplet(hash);
            if (!string.IsNullOrEmpty(triplet))
            {
                for (int i = 1; i <= 1000; i++)
                {
                    string nextHash = GetMD5Hash(salt + (index + i));
                    if (nextHash.Contains(new string(triplet[0], 5)))
                    {
                        keys++;
                        break;
                    }
                }
            }
            index++;
        }
        Console.WriteLine(index - 1);
    }

    static string GetMD5Hash(string input)
    {
        using (MD5 md5 = MD5.Create())
        {
            byte[] inputBytes = Encoding.ASCII.GetBytes(input);
            byte[] hashBytes = md5.ComputeHash(inputBytes);
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < hashBytes.Length; i++)
            {
                sb.Append(hashBytes[i].ToString("x2"));
            }
            return sb.ToString();
        }
    }

    static string FindTriplet(string hash)
    {
        for (int i = 0; i < hash.Length - 2; i++)
        {
            if (hash[i] == hash[i + 1] && hash[i] == hash[i + 2])
            {
                return hash[i].ToString();
            }
        }
        return null;
    }
}
