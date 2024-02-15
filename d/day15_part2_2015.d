
import std.stdio;
import std.algorithm;
import std.array;
import std.conv;
import std.string;
import std.file;

struct Ingredient {
    string name;
    int capacity;
    int durability;
    int flavor;
    int texture;
    int calories;
}

void main() {
    auto lines = readText("input.txt").splitter("\n").array;
    Ingredient[] ingredients;
    foreach (line; lines) {
        auto parts = line.split(": ").array;
        auto name = parts[0];
        auto props = parts[1].split(", ").array;
        auto capacity = props[0].split(" ")[1].to!int;
        auto durability = props[1].split(" ")[1].to!int;
        auto flavor = props[2].split(" ")[1].to!int;
        auto texture = props[3].split(" ")[1].to!int;
        auto calories = props[4].split(" ")[1].to!int;
        ingredients ~= Ingredient(name, capacity, durability, flavor, texture, calories);
    }

    int maxScore = 0;
    int maxScore500Cal = 0;

    foreach (i; 0 .. 101) {
        foreach (j; 0 .. 101 - i) {
            foreach (k; 0 .. 101 - i - j) {
                int l = 100 - i - j - k;
                int capacity = max(0, i * ingredients[0].capacity + j * ingredients[1].capacity + k * ingredients[2].capacity + l * ingredients[3].capacity);
                int durability = max(0, i * ingredients[0].durability + j * ingredients[1].durability + k * ingredients[2].durability + l * ingredients[3].durability);
                int flavor = max(0, i * ingredients[0].flavor + j * ingredients[1].flavor + k * ingredients[2].flavor + l * ingredients[3].flavor);
                int texture = max(0, i * ingredients[0].texture + j * ingredients[1].texture + k * ingredients[2].texture + l * ingredients[3].texture);
                int calories = i * ingredients[0].calories + j * ingredients[1].calories + k * ingredients[2].calories + l * ingredients[3].calories;

                int score = capacity * durability * flavor * texture;
                if (score > maxScore) {
                    maxScore = score;
                }

                if (calories == 500 && score > maxScore500Cal) {
                    maxScore500Cal = score;
                }
            }
        }
    }

    writeln("Part One: ", maxScore);
    writeln("Part Two: ", maxScore500Cal);
}
