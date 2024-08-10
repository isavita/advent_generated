#import <Foundation/Foundation.h>

typedef struct {
    int x;
    int y;
    int direction; // 0: East, 1: South, 2: West, 3: North
} Ship;

typedef struct {
    int x;
    int y;
} Waypoint;

void moveShip(Ship *ship, char action, int value) {
    switch (action) {
        case 'N':
            ship->y += value;
            break;
        case 'S':
            ship->y -= value;
            break;
        case 'E':
            ship->x += value;
            break;
        case 'W':
            ship->x -= value;
            break;
        case 'L':
            ship->direction = (ship->direction - value / 90 + 4) % 4;
            break;
        case 'R':
            ship->direction = (ship->direction + value / 90) % 4;
            break;
        case 'F':
            switch (ship->direction) {
                case 0:
                    ship->x += value;
                    break;
                case 1:
                    ship->y -= value;
                    break;
                case 2:
                    ship->x -= value;
                    break;
                case 3:
                    ship->y += value;
                    break;
            }
            break;
    }
}

void moveWaypoint(Waypoint *waypoint, char action, int value) {
    switch (action) {
        case 'N':
            waypoint->y += value;
            break;
        case 'S':
            waypoint->y -= value;
            break;
        case 'E':
            waypoint->x += value;
            break;
        case 'W':
            waypoint->x -= value;
            break;
        case 'L':
        case 'R': {
            int times = (action == 'L' ? 360 - value : value) / 90;
            for (int i = 0; i < times; i++) {
                int temp = waypoint->x;
                waypoint->x = waypoint->y;
                waypoint->y = -temp;
            }
            break;
        }
    }
}

void moveShipToWaypoint(Ship *ship, Waypoint *waypoint, int value) {
    ship->x += waypoint->x * value;
    ship->y += waypoint->y * value;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];

        Ship ship = {0, 0, 0};
        Waypoint waypoint = {10, 1};

        for (NSString *line in lines) {
            if (line.length == 0) continue;
            char action = [line characterAtIndex:0];
            int value = [[line substringFromIndex:1] intValue];

            moveShip(&ship, action, value);
            if (action == 'F') {
                moveShipToWaypoint(&ship, &waypoint, value);
            } else {
                moveWaypoint(&waypoint, action, value);
            }
        }

        int manhattanDistancePart1 = abs(ship.x) + abs(ship.y);
        NSLog(@"Manhattan Distance Part 1: %d", manhattanDistancePart1);

        Ship shipPart2 = {0, 0, 0};
        waypoint = (Waypoint){10, 1};

        for (NSString *line in lines) {
            if (line.length == 0) continue;
            char action = [line characterAtIndex:0];
            int value = [[line substringFromIndex:1] intValue];

            if (action == 'F') {
                moveShipToWaypoint(&shipPart2, &waypoint, value);
            } else {
                moveWaypoint(&waypoint, action, value);
            }
        }

        int manhattanDistancePart2 = abs(shipPart2.x) + abs(shipPart2.y);
        NSLog(@"Manhattan Distance Part 2: %d", manhattanDistancePart2);
    }
    return 0;
}