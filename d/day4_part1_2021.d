
import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.array;

struct BingoBoard {
    int[5][5] numbers;
    bool[5][5] marked;

    bool mark(int number) {
        foreach (r, row; numbers) {
            foreach (c, val; row) {
                if (val == number) {
                    marked[r][c] = true;
                    bool rowWon = true, colWon = true;
                    foreach (i; 0..5) {
                        if (!marked[r][i]) rowWon = false;
                        if (!marked[i][c]) colWon = false;
                    }
                    return rowWon || colWon;
                }
            }
        }
        return false;
    }

    int unmarkedSum() {
        int total = 0;
        foreach (r, row; numbers) {
            foreach (c, val; row) {
                if (!marked[r][c]) {
                    total += val;
                }
            }
        }
        return total;
    }
}

void main() {
    auto file = File("input.txt", "r");
    auto draws = file.readln().strip().split(',').map!(to!int).array;

    auto boards = appender!(BingoBoard[]);
    while (!file.eof) {
        file.readln();
        if (file.eof) break;

        BingoBoard board;
        foreach (i; 0..5) {
            board.numbers[i][] = file.readln().strip().split().map!(to!int).array;
        }
        boards.put(board);
    }

    foreach (draw; draws) {
        foreach (ref board; boards.data) {
            if (board.mark(draw)) {
                writeln(board.unmarkedSum() * draw);
                return;
            }
        }
    }
}
