using System;
using System.IO;
using System.Text;
using System.Security.Cryptography;

class Program
{
    const int CacheSize = 40000;
    static readonly string[] cache = new string[CacheSize];
    static readonly MD5 md5 = MD5.Create();

    static string Md5Hex(string s)
    {
        var bytes = Encoding.UTF8.GetBytes(s);
        var hash = md5.ComputeHash(bytes);
        var sb = new StringBuilder(32);
        foreach (var b in hash) sb.Append(b.ToString("x2"));
        return sb.ToString();
    }

    static string GetStretched(string salt, int idx)
    {
        if (idx < CacheSize && cache[idx] != null) return cache[idx];
        var h = Md5Hex(salt + idx);
        for (int i = 0; i < 2016; i++) h = Md5Hex(h);
        if (idx < CacheSize) cache[idx] = h;
        return h;
    }

    static void Main()
    {
        var salt = File.ReadAllText("input.txt").TrimEnd('\r', '\n');
        int found = 0, index = 0;
        while (true)
        {
            var hash = GetStretched(salt, index);
            char trip = '\0';
            for (int i = 0; i < 30; i++)
                if (hash[i] == hash[i + 1] && hash[i] == hash[i + 2]) { trip = hash[i]; break; }
            if (trip != '\0')
            {
                var quint = new string(trip, 5);
                for (int j = 1; j <= 1000; j++)
                {
                    if (GetStretched(salt, index + j).Contains(quint))
                    {
                        if (++found == 64) { Console.WriteLine(index); return; }
                        break;
                    }
                }
            }
            index++;
        }
    }
}