
using System;
using System.IO;

class LLNode
{
    public int elfNum;
    public int presents;
    public LLNode next;
}

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        int ans = Elephant(input);
        Console.WriteLine(ans);
    }

    static int Elephant(string input)
    {
        int startingElves = int.Parse(input);
        LLNode root = new LLNode { elfNum = 1, presents = 1 };
        LLNode iter = root;

        for (int i = 2; i <= startingElves; i++)
        {
            iter.next = new LLNode { elfNum = i, presents = 1 };
            iter = iter.next;
        }
        iter.next = root;

        bool isOddLength = startingElves % 2 == 1;
        LLNode beforeAcross = root;
        for (int i = 0; i < startingElves / 2 - 1; i++)
        {
            beforeAcross = beforeAcross.next;
        }

        while (root.next != root)
        {
            root.presents += beforeAcross.next.presents;
            beforeAcross.next = beforeAcross.next.next;

            if (isOddLength)
            {
                beforeAcross = beforeAcross.next;
            }
            isOddLength = !isOddLength;
            root = root.next;
        }

        return root.elfNum;
    }
}
