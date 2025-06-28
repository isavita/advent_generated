
import std.stdio;
import std.file;
import std.regex;
import std.conv;

long power(long base, long exp, long mod) {
    long res = 1;
    base %= mod;
    while (exp > 0) {
        if (exp % 2 == 1) res = (res * base) % mod;
        base = (base * base) % mod;
        exp /= 2;
    }
    return res;
}

long get_position(int row, int column) {
    long r = row;
    long c = column;
    return (r + c - 2) * (r + c - 1) / 2 + c;
}

long get_code(long position) {
    long startCode = 20151125;
    long multiplier = 252533;
    long modulus = 33554393;

    if (position == 1) {
        return startCode;
    }

    long expTerm = power(multiplier, position - 1, modulus);
    return (startCode * expTerm) % modulus;
}

void main() {
    string data = readText("input.txt");

    auto re = regex(r"row (\d+), column (\d+)");
    auto m = matchFirst(data, re);

    if (!m) {
        throw new Exception("Invalid input format.");
    }

    int row = to!int(m.captures[1]);
    int column = to!int(m.captures[2]);

    long pos = get_position(row, column);
    long code = get_code(pos);

    writeln(code);
}
