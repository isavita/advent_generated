
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        int result = Solve(input);
        Console.WriteLine(result);
    }

    static int Solve(string input)
    {
        var (nums, boards) = ParseInput(input);

        int lastWinningScore = -1;
        var alreadyWon = new Dictionary<int, bool>();
        foreach (var n in nums)
        {
            foreach (var (bi, b) in boards.Select((value, index) => (index, value)))
            {
                if (alreadyWon.ContainsKey(bi))
                {
                    continue;
                }

                bool didWin = b.PickNum(n);
                if (didWin)
                {
                    lastWinningScore = b.Score() * n;
                    alreadyWon[bi] = true;
                }
            }
        }

        return lastWinningScore;
    }

    class BoardState
    {
        private int[][] board;
        private bool[][] picked;

        public BoardState(int[][] board)
        {
            this.board = board;
            this.picked = new bool[board.Length][];
            for (int i = 0; i < board.Length; i++)
            {
                this.picked[i] = new bool[board[0].Length];
            }
        }

        public bool PickNum(int num)
        {
            for (int r = 0; r < board.Length; r++)
            {
                for (int c = 0; c < board[0].Length; c++)
                {
                    if (board[r][c] == num)
                    {
                        picked[r][c] = true;
                    }
                }
            }

            for (int i = 0; i < board.Length; i++)
            {
                bool isFullRow = true;
                bool isFullCol = true;

                for (int j = 0; j < board.Length; j++)
                {
                    if (!picked[i][j])
                    {
                        isFullRow = false;
                    }

                    if (!picked[j][i])
                    {
                        isFullCol = false;
                    }
                }

                if (isFullRow || isFullCol)
                {
                    return true;
                }
            }

            return false;
        }

        public int Score()
        {
            int score = 0;

            for (int r = 0; r < board.Length; r++)
            {
                for (int c = 0; c < board[0].Length; c++)
                {
                    if (!picked[r][c])
                    {
                        score += board[r][c];
                    }
                }
            }

            return score;
        }
    }

    static (int[], BoardState[]) ParseInput(string input)
    {
        var lines = input.Split("\n\n");

        var nums = lines[0].Split(',').Select(int.Parse).ToArray();

        var boards = new List<BoardState>();
        foreach (var grid in lines.Skip(1))
        {
            var b = grid.Split('\n')
                        .Select(line => line.Replace("  ", " ").TrimStart())
                        .Select(line => line.Split(' ').Select(int.Parse).ToArray())
                        .ToArray();

            boards.Add(new BoardState(b));
        }

        return (nums, boards.ToArray());
    }
}
