
import std.stdio;
import std.file;
import std.string;
import std.array;
import std.algorithm;

enum Direction { up, down, left, right }
struct State { int x, y; Direction dir; }

long calculateEnergized(const string[] grid, State startState) {
    immutable int width = cast(int)grid[0].length;
    immutable int height = cast(int)grid.length;

    auto queue = [startState];
    bool[State] visited;

    while (!queue.empty) {
        auto beam = queue[0];
        queue = queue[1 .. $];

        if (beam.x < 0 || beam.x >= width || beam.y < 0 || beam.y >= height) {
            continue;
        }
        if (beam in visited) {
            continue;
        }
        visited[beam] = true;

        char tile = grid[beam.y][beam.x];
        auto dir = beam.dir;

        if (tile == '.' ||
            (tile == '-' && (dir == Direction.left || dir == Direction.right)) ||
            (tile == '|' && (dir == Direction.up || dir == Direction.down))) {
            int nextX = beam.x, nextY = beam.y;
            final switch (dir) {
                case Direction.up:    nextY--; break;
                case Direction.down:  nextY++; break;
                case Direction.left:  nextX--; break;
                case Direction.right: nextX++; break;
            }
            queue ~= State(nextX, nextY, dir);
        } else if (tile == '/') {
            final switch (dir) {
                case Direction.up:    queue ~= State(beam.x + 1, beam.y, Direction.right); break;
                case Direction.down:  queue ~= State(beam.x - 1, beam.y, Direction.left);  break;
                case Direction.left:  queue ~= State(beam.x, beam.y + 1, Direction.down);  break;
                case Direction.right: queue ~= State(beam.x, beam.y - 1, Direction.up);    break;
            }
        } else if (tile == '\\') {
            final switch (dir) {
                case Direction.up:    queue ~= State(beam.x - 1, beam.y, Direction.left);  break;
                case Direction.down:  queue ~= State(beam.x + 1, beam.y, Direction.right); break;
                case Direction.left:  queue ~= State(beam.x, beam.y - 1, Direction.up);    break;
                case Direction.right: queue ~= State(beam.x, beam.y + 1, Direction.down);  break;
            }
        } else if (tile == '|') {
            queue ~= State(beam.x, beam.y - 1, Direction.up);
            queue ~= State(beam.x, beam.y + 1, Direction.down);
        } else if (tile == '-') {
            queue ~= State(beam.x - 1, beam.y, Direction.left);
            queue ~= State(beam.x + 1, beam.y, Direction.right);
        }
    }

    bool[int[2]] energized;
    foreach (state; visited.keys) {
        energized[[state.x, state.y]] = true;
    }
    return energized.length;
}

void main() {
    auto grid = readText("input.txt").strip.splitLines;
    immutable int width = cast(int)grid[0].length;
    immutable int height = cast(int)grid.length;

    long maxEnergized = 0;

    foreach (x; 0 .. width) {
        maxEnergized = max(maxEnergized, calculateEnergized(grid, State(x, 0, Direction.down)));
        maxEnergized = max(maxEnergized, calculateEnergized(grid, State(x, height - 1, Direction.up)));
    }

    foreach (y; 0 .. height) {
        maxEnergized = max(maxEnergized, calculateEnergized(grid, State(0, y, Direction.right)));
        maxEnergized = max(maxEnergized, calculateEnergized(grid, State(width - 1, y, Direction.left)));
    }

    writeln(maxEnergized);
}
