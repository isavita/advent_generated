
import std.stdio;
import std.file;
import std.conv;

struct Ship {
	int x, y;
	int facing;
}

void main() {
	auto file = File("input.txt", "r");
	auto ship = Ship(0, 0, 0);

	foreach(line; file.byLine()) {
		auto action = line[0];
		auto value = to!int(line[1..$]);
		processInstruction(ship, action, value);
	}

	auto manhattanDistance = abs(ship.x) + abs(ship.y);
	writeln(manhattanDistance);
}

void processInstruction(ref Ship ship, char action, int value) {
	switch (action) {
		case 'N':
			ship.y += value;
			break;
		case 'S':
			ship.y -= value;
			break;
		case 'E':
			ship.x += value;
			break;
		case 'W':
			ship.x -= value;
			break;
		case 'L':
			ship.facing = (ship.facing - value + 360) % 360;
			break;
		case 'R':
			ship.facing = (ship.facing + value) % 360;
			break;
		case 'F':
			switch (ship.facing) {
				case 0:
					ship.x += value;
					break;
				case 90:
					ship.y -= value;
					break;
				case 180:
					ship.x -= value;
					break;
				case 270:
					ship.y += value;
					break;
				default:
					assert(0, "Invalid facing direction");
			}
			break;
		default:
			assert(0, "Invalid action");
	}
}

int abs(int x) {
	return x < 0 ? -x : x;
}
