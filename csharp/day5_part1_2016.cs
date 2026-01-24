using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;

class Program
{
    static void Main()
    {
        var doorId = File.ReadAllText("input.txt").TrimEnd('\r', '\n');
        var md5 = MD5.Create();
        var sb = new StringBuilder(8);
        var buffer = new StringBuilder();
        for (ulong i = 0; sb.Length < 8; i++)
        {
            buffer.Clear();
            buffer.Append(doorId).Append(i);
            var hash = md5.ComputeHash(Encoding.UTF8.GetBytes(buffer.ToString()));
            if (hash[0] != 0 || hash[1] != 0 || (hash[2] & 0xF0) != 0) continue;
            var v = hash[2] & 0x0F;
            sb.Append((char)(v < 10 ? '0' + v : 'a' + v - 10));
        }
        Console.WriteLine(sb.ToString());
    }
}
