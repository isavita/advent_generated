
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    struct Node
    {
        public int Pos;
        public long Val;
    }

    static void Mix(Node[] nums)
    {
        int n = nums.Length - 1;
        for (int i = 0; i < nums.Length; i++)
        {
            int oldPos = nums[i].Pos;
            long offset = nums[i].Val % n;
            if (offset < 0) offset += n;
            int newPos = (int)((oldPos + offset) % n);

            if (oldPos < newPos)
            {
                for (int j = 0; j < nums.Length; j++)
                    if (nums[j].Pos > oldPos && nums[j].Pos <= newPos)
                        nums[j].Pos--;
            }
            else if (newPos < oldPos)
            {
                for (int j = 0; j < nums.Length; j++)
                    if (nums[j].Pos >= newPos && nums[j].Pos < oldPos)
                        nums[j].Pos++;
            }
            nums[i].Pos = newPos;
        }
    }

    static long Coords(Node[] nums)
    {
        int l = nums.Length;
        int zeroPos = Array.Find(nums, x => x.Val == 0).Pos;
        long sum = 0;
        for (int i = 1000; i <= 3000; i += 1000)
            sum += Array.Find(nums, x => x.Pos == (zeroPos + i) % l).Val;
        return sum;
    }

    static void Main()
    {
        long[] raw = File.ReadAllLines("input.txt").Select(long.Parse).ToArray();
        Node[] nums = new Node[raw.Length];
        for (int i = 0; i < raw.Length; i++)
            nums[i] = new Node { Pos = i, Val = raw[i] * 811589153L };

        for (int r = 0; r < 10; r++)
            Mix(nums);

        Console.WriteLine(Coords(nums));
    }
}
