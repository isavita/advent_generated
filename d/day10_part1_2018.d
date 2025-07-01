
import std.stdio;
import std.file;
import std.string;
import std.algorithm;
import core.stdc.stdio;

struct Star {
    int x, y, vX, vY;
}

void main() {
    Star[] stars;
    foreach (line; readText("input.txt").strip.split("\n")) {
        int x, y, vX, vY;
        sscanf(line.ptr, "position=<%d, %d> velocity=<%d, %d>", &x, &y, &vX, &vY);
        stars ~= Star(x, y, vX, vY);
    }

    long smallestArea = long.max;
    int smallestT = 0;

    foreach (t; 1 .. 20_000) {
        int tMinX = int.max, tMaxX = int.min;
        int tMinY = int.max, tMaxY = int.min;

        foreach (ref star; stars) {
            int curX = star.x + star.vX * t;
            int curY = star.y + star.vY * t;
            tMinX = min(tMinX, curX);
            tMaxX = max(tMaxX, curX);
            tMinY = min(tMinY, curY);
            tMaxY = max(tMaxY, curY);
        }

        long area = cast(long)(tMaxX - tMinX) + (tMaxY - tMinY);
        if (area < smallestArea) {
            smallestArea = area;
            smallestT = t;
        } else {
            break;
        }
    }

    int minX = int.max, maxX = int.min;
    int minY = int.max, maxY = int.min;

    foreach (ref star; stars) {
        star.x += star.vX * smallestT;
        star.y += star.vY * smallestT;
        minX = min(minX, star.x);
        maxX = max(maxX, star.x);
        minY = min(minY, star.y);
        maxY = max(maxY, star.y);
    }

    auto grid = new char[][](maxY - minY + 1, maxX - minX + 1);
    foreach (row; grid) {
        row[] = ' ';
    }

    foreach (ref star; stars) {
        grid[star.y - minY][star.x - minX] = '#';
    }

    foreach (row; grid) {
        writeln(row);
    }
}
