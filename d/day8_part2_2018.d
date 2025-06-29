
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.array;
import std.algorithm;
import std.range;

int parseNode(ref int[] data, ref int totalMetadataSum) {
    int numChildren = data[0];
    int numMetadata = data[1];
    data = data[2..$];

    if (numChildren == 0) {
        auto metadata = data[0 .. numMetadata];
        int metadataSum = metadata.sum();
        totalMetadataSum += metadataSum;
        data = data[numMetadata .. $];
        return metadataSum;
    } else {
        auto childValues = new int[numChildren];
        foreach (i; 0 .. numChildren) {
            childValues[i] = parseNode(data, totalMetadataSum);
        }

        int nodeValue = 0;
        auto metadata = data[0 .. numMetadata];
        totalMetadataSum += metadata.sum();

        foreach (idx; metadata) {
            if (idx > 0 && idx <= numChildren) {
                nodeValue += childValues[idx - 1];
            }
        }
        data = data[numMetadata .. $];
        return nodeValue;
    }
}

void main() {
    auto data = readText("input.txt")
                .strip
                .split
                .map!(to!int)
                .array;

    int totalMetadataSum = 0;
    int rootValue = parseNode(data, totalMetadataSum);

    writeln(totalMetadataSum);
    writeln(rootValue);
}
