using System;
using System.IO;

class Program
{
    static bool Valid(int a, int b, int c) => a + b > c && a + c > b && b + c > a;

    static void Main()
    {
        var s = File.ReadAllText("input.txt");
        int i = 0, n = s.Length, count = 0;
        while (i < n)
        {
            while (i < n && s[i] <= ' ') i++;
            int a = 0;
            while (i < n && s[i] > ' ') { a = a * 10 + (s[i++] - '0'); }
            while (i < n && s[i] <= ' ') i++;
            int b = 0;
            while (i < n && s[i] > ' ') { b = b * 10 + (s[i++] - '0'); }
            while (i < n && s[i] <= ' ') i++;
            int c = 0;
            while (i < n && s[i] > ' ') { c = c * 10 + (s[i++] - '0'); }
            if (Valid(a, b, c)) count++;
        }
        Console.WriteLine(count);
    }
}
