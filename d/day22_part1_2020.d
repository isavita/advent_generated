
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.stdio; // Added import for writeln

void main()
{
    string input = cast(string) read("input.txt");
    auto sections = input.split("\n\n");

    auto player1 = sections[0].split("\n")[1..$].map!(to!int).array;
    auto player2 = sections[1].split("\n")[1..$].map!(to!int).array;

    while (!player1.empty && !player2.empty)
    {
        int card1 = player1.front;
        int card2 = player2.front;
        player1 = player1[1..$];
        player2 = player2[1..$];

        if (card1 > card2)
        {
            player1 ~= card1;
            player1 ~= card2;
        }
        else
        {
            player2 ~= card2;
            player2 ~= card1;
        }
    }

    auto winningDeck = player1.empty ? player2 : player1;

    int score = 0;
    foreach_reverse(i, card; winningDeck)
    {
        score += card * (winningDeck.length - i);
    }

    writeln(score);
}
