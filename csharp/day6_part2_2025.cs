
using System;
using System.IO;
using System.Linq;

public class Program
{
    public static string AddStrings(string a, string b)
    {
        var maxLen = Math.Max(a.Length, b.Length);
        var result = new char[maxLen + 1];
        var carry = 0;
        for (var i = 0; i < maxLen; i++)
        {
            var da = i < a.Length ? a[a.Length - 1 - i] - '0' : 0;
            var db = i < b.Length ? b[b.Length - 1 - i] - '0' : 0;
            var sum = da + db + carry;
            result[maxLen - i] = (char)((sum % 10) + '0');
            carry = sum / 10;
        }
        if (carry > 0)
        {
            result[0] = (char)(carry + '0');
            return new string(result);
        }
        return new string(result, 1, maxLen);
    }

    public static string MultiplyStrings(string a, string b)
    {
        if (a == "0" || b == "0") return "0";

        var la = a.Length;
        var lb = b.Length;
        var tmp = new int[la + lb];
        for (var i = 0; i < la; i++)
        {
            var da = a[la - 1 - i] - '0';
            for (var j = 0; j < lb; j++)
            {
                var db = b[lb - 1 - j] - '0';
                tmp[i + j] += da * db;
            }
        }
        var carry = 0;
        for (var k = 0; k < la + lb; k++)
        {
            var sum = tmp[k] + carry;
            tmp[k] = sum % 10;
            carry = sum / 10;
        }
        var len = la + lb;
        while (len > 1 && tmp[len - 1] == 0) len--;
        var res = new char[len];
        for (var i = 0; i < len; i++)
            res[i] = (char)(tmp[len - 1 - i] + '0');
        return new string(res);
    }

    public static string ProcessBlock(string[] lines, int start, int end)
    {
        var nums = new System.Collections.Generic.List<string>();
        var op = '+';
        for (var c = start; c <= end; c++)
        {
            var buf = "";
            for (var r = 0; r < lines.Length; r++)
            {
                if (c < lines[r].Length)
                {
                    var ch = lines[r][c];
                    if (char.IsDigit(ch))
                        buf += ch;
                    else if (ch == '+' || ch == '*')
                        op = ch;
                }
            }
            if (buf.Length > 0)
                nums.Add(buf);
        }
        if (nums.Count == 0) return "0";

        if (op == '*')
        {
            var blockRes = "1";
            foreach (var num in nums)
                blockRes = MultiplyStrings(blockRes, num);
            return blockRes;
        }
        else
        {
            var blockRes = "0";
            foreach (var num in nums)
                blockRes = AddStrings(blockRes, num);
            return blockRes;
        }
    }

    public static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        if (lines.Length == 0)
        {
            Console.WriteLine("Grand total: 0");
            return;
        }

        var maxw = lines.Max(l => l.Length);
        var isSep = new bool[maxw];
        for (var x = 0; x < maxw; x++)
        {
            var allspace = true;
            for (var r = 0; r < lines.Length; r++)
            {
                if (x < lines[r].Length && !char.IsWhiteSpace(lines[r][x]))
                {
                    allspace = false;
                    break;
                }
            }
            isSep[x] = allspace;
        }

        var grandTotal = "0";
        var inBlock = false;
        var start = 0;
        for (var x = 0; x < maxw; x++)
        {
            if (!isSep[x])
            {
                if (!inBlock)
                {
                    inBlock = true;
                    start = x;
                }
            }
            else
            {
                if (inBlock)
                {
                    grandTotal = AddStrings(grandTotal, ProcessBlock(lines, start, x - 1));
                    inBlock = false;
                }
            }
        }
        if (inBlock)
            grandTotal = AddStrings(grandTotal, ProcessBlock(lines, start, maxw - 1));

        Console.WriteLine($"Grand total: {grandTotal}");
    }
}
