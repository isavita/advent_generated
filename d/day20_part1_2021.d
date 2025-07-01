
import std.stdio;
import std.file;
import std.string;
import std.algorithm;
import std.range;

void main() {
    auto blocks = readText("input.txt").strip().split("\n\n");
    auto algorithm = blocks[0];
    auto image = blocks[1].split("\n").map!(a => a.dup).array;

    foreach (i; 0 .. 2) {
        bool useInfinitePlane = (algorithm[0] == '#') && (i % 2 == 1);
        size_t h = image.length;
        size_t w = image[0].length;
        auto newImage = new char[][](h + 2, w + 2);

        foreach (r; 0 .. h + 2) {
            foreach (c; 0 .. w + 2) {
                uint index = 0;
                foreach (dr; -1 .. 2) {
                    foreach (dc; -1 .. 2) {
                        index <<= 1;
                        auto nr = r - 1 + dr;
                        auto nc = c - 1 + dc;
                        if (nr >= 0 && nr < h && nc >= 0 && nc < w) {
                            if (image[nr][nc] == '#') {
                                index |= 1;
                            }
                        } else if (useInfinitePlane) {
                            index |= 1;
                        }
                    }
                }
                newImage[r][c] = algorithm[index];
            }
        }
        image = newImage;
    }

    writeln(image.joiner.count!q{a == '#'});
}
