
import std.stdio, std.file, std.string, std.conv, std.bigint, std.algorithm;

enum long mod = 1L << 24;
enum int numSteps = 2000;

long nextSecret(long s) {
    s ^= s * 64;
    s &= mod - 1;
    s ^= s / 32;
    s &= mod - 1;
    s ^= s * 2048;
    s &= mod - 1;
    return s;
}

size_t encodeChange4(int c1, int c2, int c3, int c4) {
    return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19;
}

void main() {
    auto initials = readText("input.txt").strip().splitLines().map!(to!long);

    enum size_t patternCount = 19 * 19 * 19 * 19;
    auto globalSum = new BigInt[patternCount];

    auto prices = new int[numSteps + 1];
    auto changes = new int[numSteps];
    auto localPrice = new int[patternCount];

    foreach (initVal; initials) {
        long s = initVal;
        foreach (j; 0 .. numSteps + 1) {
            prices[j] = cast(int)(s % 10);
            if (j < numSteps) {
                s = nextSecret(s);
            }
        }

        foreach (j; 0 .. numSteps) {
            changes[j] = prices[j + 1] - prices[j];
        }

        localPrice[] = -1;
        foreach (i; 0 .. numSteps - 3) {
            auto idx = encodeChange4(changes[i], changes[i + 1], changes[i + 2], changes[i + 3]);
            if (localPrice[idx] < 0) {
                localPrice[idx] = prices[i + 4];
            }
        }

        foreach (idx, price; localPrice) {
            if (price >= 0) {
                globalSum[idx] += price;
            }
        }
    }

    writeln(maxElement(globalSum));
}
