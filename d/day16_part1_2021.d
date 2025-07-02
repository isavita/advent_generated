
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;

struct BitStream {
    const(ubyte)[] data;
    size_t pos;

    this(string hex) {
        auto nibbles = new ubyte[hex.length];
        foreach (i, char c; hex) {
            if (c >= '0' && c <= '9') {
                nibbles[i] = cast(ubyte)(c - '0');
            } else {
                nibbles[i] = cast(ubyte)(c - 'A' + 10);
            }
        }
        this.data = nibbles;
    }

    ulong read(int n) {
        ulong result = 0;
        while (n > 0) {
            auto nibbleIndex = pos / 4;
            auto bitsInNibble = 4 - (pos % 4);
            auto bitsToRead = min(n, cast(int)bitsInNibble);
            
            auto mask = (1 << bitsToRead) - 1;
            auto shift = bitsInNibble - bitsToRead;
            auto value = (data[nibbleIndex] >> shift) & mask;
            
            result = (result << bitsToRead) | value;
            pos += bitsToRead;
            n -= bitsToRead;
        }
        return result;
    }
}

long parsePacket(ref BitStream stream) {
    long versionSum = stream.read(3);
    long typeID = stream.read(3);

    if (typeID == 4) {
        while (stream.read(1) == 1) {
            stream.read(4);
        }
        stream.read(4);
    } else {
        auto lengthTypeID = stream.read(1);
        if (lengthTypeID == 0) {
            auto totalLength = stream.read(15);
            auto endPos = stream.pos + totalLength;
            while (stream.pos < endPos) {
                versionSum += parsePacket(stream);
            }
        } else {
            auto numSubPackets = stream.read(11);
            foreach (i; 0 .. numSubPackets) {
                versionSum += parsePacket(stream);
            }
        }
    }
    return versionSum;
}

void main() {
    auto hexStr = readText("input.txt").strip;
    auto stream = BitStream(hexStr);
    writeln(parsePacket(stream));
}
