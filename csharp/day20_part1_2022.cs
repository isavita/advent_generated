
using System;
using System.IO;
using System.Linq;

class Program
{
    struct Num
    {
        public int Pos;
        public int Val;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Num[] nums = lines.Select((line, index) => new Num { Pos = index, Val = int.Parse(line) }).ToArray();
        Num[] nums2 = new Num[nums.Length];
        for (int i = 0; i < nums.Length; i++)
        {
            nums2[i] = new Num { Pos = nums[i].Pos, Val = 811589153 * nums[i].Val };
        }

        Mix(nums);
        Console.WriteLine(Coords(nums));
    }

    static void Mix(Num[] nums)
    {
        int n = nums.Length - 1;
        for (int i = 0; i < nums.Length; i++)
        {
            int oldPos = nums[i].Pos;
            int newPos = ((oldPos + nums[i].Val) % n + n) % n;
            if (oldPos < newPos)
            {
                for (int j = 0; j < nums.Length; j++)
                {
                    if (nums[j].Pos > oldPos && nums[j].Pos <= newPos)
                    {
                        nums[j].Pos--;
                    }
                }
            }
            if (newPos < oldPos)
            {
                for (int j = 0; j < nums.Length; j++)
                {
                    if (nums[j].Pos >= newPos && nums[j].Pos < oldPos)
                    {
                        nums[j].Pos++;
                    }
                }
            }
            nums[i].Pos = newPos;
        }
    }

    static int Coords(Num[] nums)
    {
        int l = nums.Length;
        int zeroPos = 0;
        foreach (var num in nums)
        {
            if (num.Val == 0)
            {
                zeroPos = num.Pos;
                break;
            }
        }

        int sum = 0;
        foreach (var num in nums)
        {
            if (num.Pos == (zeroPos + 1000) % l || num.Pos == (zeroPos + 2000) % l || num.Pos == (zeroPos + 3000) % l)
            {
                sum += num.Val;
            }
        }

        return sum;
    }
}
