
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        List<string> lines = new List<string>(File.ReadAllLines("input.txt"));
        List<char[]> track = new List<char[]>();
        List<Cart> carts = new List<Cart>();

        for (int i = 0; i < lines.Count; i++)
        {
            track.Add(new char[lines[i].Length]);
            for (int j = 0; j < lines[i].Length; j++)
            {
                char s = lines[i][j];
                switch (s)
                {
                    case '>':
                        track[i][j] = '-';
                        carts.Add(new Cart(j, i, '>'));
                        break;
                    case '<':
                        track[i][j] = '-';
                        carts.Add(new Cart(j, i, '<'));
                        break;
                    case '^':
                        track[i][j] = '|';
                        carts.Add(new Cart(j, i, '^'));
                        break;
                    case 'v':
                        track[i][j] = '|';
                        carts.Add(new Cart(j, i, 'v'));
                        break;
                    default:
                        track[i][j] = s;
                        break;
                }
            }
        }

        bool collision = false;
        while (!collision)
        {
            for (int i = 0; i < carts.Count; i++)
            {
                switch (carts[i].dir)
                {
                    case '>':
                        carts[i] = MovingRight(track, carts[i]);
                        break;
                    case '<':
                        carts[i] = MovingLeft(track, carts[i]);
                        break;
                    case '^':
                        carts[i] = MovingUp(track, carts[i]);
                        break;
                    case 'v':
                        carts[i] = MovingDown(track, carts[i]);
                        break;
                    default:
                        Console.WriteLine("error not valid cart");
                        break;
                }
            }

            for (int i = 0; i < carts.Count; i++)
            {
                for (int j = i + 1; j < carts.Count; j++)
                {
                    if (carts[i].x == carts[j].x && carts[i].y == carts[j].y)
                    {
                        collision = true;
                        Console.WriteLine(carts[i].x + "," + carts[i].y);
                    }
                }
            }
        }
    }

    static void PrintTrack(List<char[]> track, List<Cart> carts)
    {
        List<char[]> h = new List<char[]>();

        for (int i = 0; i < track.Count; i++)
        {
            h.Add(new char[track[i].Length]);
            Array.Copy(track[i], h[i], track[i].Length);
        }

        foreach (Cart cart in carts)
        {
            h[cart.y][cart.x] = cart.dir;
        }

        foreach (char[] row in h)
        {
            Console.WriteLine(new string(row));
        }
    }

    static Cart MovingDown(List<char[]> track, Cart cart)
    {
        switch (track[cart.y + 1][cart.x])
        {
            case '/':
                cart.dir = '<';
                break;
            case '\\':
                cart.dir = '>';
                break;
            case '+':
                if (cart.turn == 0)
                {
                    cart.dir = '>';
                    cart.turn = 1;
                }
                else if (cart.turn == 1)
                {
                    cart.turn = 2;
                }
                else if (cart.turn == 2)
                {
                    cart.dir = '<';
                    cart.turn = 0;
                }
                break;
            case '|':
                break;
            default:
                Console.WriteLine("Error on track cart can't move : " + cart.x + "," + (cart.y - 1) + "," + track[cart.y - 1][cart.x]);
                break;
        }
        cart.y = cart.y + 1;
        return cart;
    }

    static Cart MovingUp(List<char[]> track, Cart cart)
    {
        switch (track[cart.y - 1][cart.x])
        {
            case '/':
                cart.dir = '>';
                break;
            case '\\':
                cart.dir = '<';
                break;
            case '+':
                if (cart.turn == 0)
                {
                    cart.dir = '<';
                    cart.turn = 1;
                }
                else if (cart.turn == 1)
                {
                    cart.turn = 2;
                }
                else if (cart.turn == 2)
                {
                    cart.dir = '>';
                    cart.turn = 0;
                }
                break;
            case '|':
                break;
            default:
                Console.WriteLine("Error on track cart can't move : " + cart.x + "," + (cart.y - 1) + "," + track[cart.y - 1][cart.x]);
                break;
        }
        cart.y = cart.y - 1;
        return cart;
    }

    static Cart MovingLeft(List<char[]> track, Cart cart)
    {
        switch (track[cart.y][cart.x - 1])
        {
            case '/':
                cart.dir = 'v';
                break;
            case '\\':
                cart.dir = '^';
                break;
            case '+':
                if (cart.turn == 0)
                {
                    cart.dir = 'v';
                    cart.turn = 1;
                }
                else if (cart.turn == 1)
                {
                    cart.turn = 2;
                }
                else if (cart.turn == 2)
                {
                    cart.dir = '^';
                    cart.turn = 0;
                }
                break;
            case '-':
                break;
            default:
                Console.WriteLine("Error on track cart can't move : " + (cart.x - 1) + "," + cart.y + "," + track[cart.y][cart.x - 1]);
                break;
        }
        cart.x = cart.x - 1;
        return cart;
    }

    static Cart MovingRight(List<char[]> track, Cart cart)
    {
        switch (track[cart.y][cart.x + 1])
        {
            case '\\':
                cart.dir = 'v';
                break;
            case '/':
                cart.dir = '^';
                break;
            case '+':
                if (cart.turn == 0)
                {
                    cart.dir = '^';
                    cart.turn = 1;
                }
                else if (cart.turn == 1)
                {
                    cart.turn = 2;
                }
                else if (cart.turn == 2)
                {
                    cart.dir = 'v';
                    cart.turn = 0;
                }
                break;
            case '-':
                break;
            default:
                Console.WriteLine("Error on track cart can't move : " + (cart.x + 1) + "," + cart.y + "," + track[cart.y][cart.x + 1]);
                break;
        }
        cart.x = cart.x + 1;
        return cart;
    }
}

class Cart
{
    public int x;
    public int y;
    public char dir;
    public int turn;

    public Cart(int x, int y, char dir)
    {
        this.x = x;
        this.y = y;
        this.dir = dir;
        this.turn = 0;
    }
}
