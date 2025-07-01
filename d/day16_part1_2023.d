
import std.stdio;
import std.file;
import std.string;
import std.container.dlist;

enum Direction { up, down, left, right }
struct Point { int x; int y; }
struct State { Point pos; Direction dir; }

void main() {
    auto grid = readText("input.txt").strip().splitLines();
    if (grid.length == 0) {
        writeln(0);
        return;
    }
    auto height = grid.length;
    auto width = grid[0].length;

    immutable Point[4] deltas = [
        Point(0, -1), // up
        Point(0, 1),  // down
        Point(-1, 0), // left
        Point(1, 0)   // right
    ];

    auto beamQueue = DList!(State)();
    beamQueue.insertBack(State(Point(-1, 0), Direction.right));

    bool[State] visited;
    bool[Point] energizedTiles;

    while (!beamQueue.empty) {
        auto beam = beamQueue.front;
        beamQueue.removeFront();

        auto delta = deltas[beam.dir];
        auto nextPos = Point(beam.pos.x + delta.x, beam.pos.y + delta.y);

        if (nextPos.x < 0 || nextPos.x >= width || nextPos.y < 0 || nextPos.y >= height) {
            continue;
        }

        auto nextState = State(nextPos, beam.dir);
        if (nextState in visited) {
            continue;
        }
        visited[nextState] = true;
        energizedTiles[nextPos] = true;

        void enqueue(Direction dir) {
            beamQueue.insertBack(State(nextPos, dir));
        }

        auto tile = grid[nextPos.y][nextPos.x];
        auto dir = beam.dir;

        switch (tile) {
            case '.':
                enqueue(dir);
                break;
            case '/':
                enqueue([Direction.right, Direction.left, Direction.down, Direction.up][dir]);
                break;
            case '\\':
                enqueue([Direction.left, Direction.right, Direction.up, Direction.down][dir]);
                break;
            case '|':
                if (dir == Direction.left || dir == Direction.right) {
                    enqueue(Direction.up);
                    enqueue(Direction.down);
                } else {
                    enqueue(dir);
                }
                break;
            case '-':
                if (dir == Direction.up || dir == Direction.down) {
                    enqueue(Direction.left);
                    enqueue(Direction.right);
                } else {
                    enqueue(dir);
                }
                break;
            default:
                break;
        }
    }

    writeln(energizedTiles.length);
}
