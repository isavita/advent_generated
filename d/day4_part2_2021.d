import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.string;
import std.algorithm;

struct Board {
    int[5][5] numbers;
    bool[5][5] marked;
    bool won;
}

void main() {
    auto file = File("input.txt", "r");
    auto lines = file.byLine.map!(a => a.idup).array;
    file.close();

    auto draws = lines[0].split(",").map!(to!int).array;

    Board[] boards;
    for (size_t i = 2; i < lines.length; i += 6) {
        Board board;
        foreach (j; 0 .. 5) {
            auto nums = lines[i + j].split(" ").filter!(a => a != "").map!(to!int).array;
            foreach (k; 0 .. 5) {
                board.numbers[j][k] = nums[k];
            }
        }
        boards ~= board;
    }

    int lastScore = 0;
    foreach (draw; draws) {
        foreach (ref board; boards) {
            if (board.won) continue;
            foreach (i; 0 .. 5) {
                foreach (j; 0 .. 5) {
                    if (board.numbers[i][j] == draw) {
                        board.marked[i][j] = true;
                    }
                }
            }
            if (checkWin(board)) {
                board.won = true;
                lastScore = calculateScore(board, draw);
            }
        }
    }

    writeln(lastScore);
}

bool checkWin(Board board) {
    foreach (i; 0 .. 5) {
        bool rowWin = true;
        bool colWin = true;
        foreach (j; 0 .. 5) {
            if (!board.marked[i][j]) rowWin = false;
            if (!board.marked[j][i]) colWin = false;
        }
        if (rowWin || colWin) return true;
    }
    return false;
}

int calculateScore(Board board, int lastDraw) {
    int sumUnmarked = 0;
    foreach (i; 0 .. 5) {
        foreach (j; 0 .. 5) {
            if (!board.marked[i][j]) {
                sumUnmarked += board.numbers[i][j];
            }
        }
    }
    return sumUnmarked * lastDraw;
}