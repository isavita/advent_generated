
using System;
using System.IO;
using System.Linq;

public class BingoBoard
{
    private const int BoardSize = 5;
    private int[,] numbers = new int[BoardSize, BoardSize];
    private bool[,] marked = new bool[BoardSize, BoardSize];

    public void Mark(int number)
    {
        for (int i = 0; i < BoardSize; i++)
        {
            for (int j = 0; j < BoardSize; j++)
            {
                if (numbers[i, j] == number)
                {
                    marked[i, j] = true;
                }
            }
        }
    }

    public bool HasWon()
    {
        // Check rows
        for (int i = 0; i < BoardSize; i++)
        {
            bool rowWon = true;
            for (int j = 0; j < BoardSize; j++)
            {
                if (!marked[i, j])
                {
                    rowWon = false;
                    break;
                }
            }
            if (rowWon) return true;
        }

        // Check columns
        for (int i = 0; i < BoardSize; i++)
        {
            bool columnWon = true;
            for (int j = 0; j < BoardSize; j++)
            {
                if (!marked[j, i])
                {
                    columnWon = false;
                    break;
                }
            }
            if (columnWon) return true;
        }

        return false;
    }

    public int UnmarkedSum()
    {
        int sum = 0;
        for (int i = 0; i < BoardSize; i++)
        {
            for (int j = 0; j < BoardSize; j++)
            {
                if (!marked[i, j])
                {
                    sum += numbers[i, j];
                }
            }
        }
        return sum;
    }

    public void ReadFromLine(string[] lines, int startIndex)
    {
        for (int i = 0; i < BoardSize; i++)
        {
            string[] parts = lines[startIndex + i].Trim().Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            for (int j = 0; j < BoardSize; j++)
            {
                numbers[i, j] = int.Parse(parts[j]);
            }
        }
    }
}

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[] numbers = lines[0].Split(',').Select(int.Parse).ToArray();

        BingoBoard[] boards = new BingoBoard[(lines.Length - 1) / 6];
        for (int i = 0; i < boards.Length; i++)
        {
            boards[i] = new BingoBoard();
            boards[i].ReadFromLine(lines, 2 + i * 6);
        }

        foreach (int number in numbers)
        {
            foreach (BingoBoard board in boards)
            {
                board.Mark(number);
                if (board.HasWon())
                {
                    Console.WriteLine(board.UnmarkedSum() * number);
                    return;
                }
            }
        }
    }
}
