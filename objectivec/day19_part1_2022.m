
#import <Foundation/Foundation.h>

typedef struct {
    int id;
    int oreForOreRobot;
    int oreForClayRobot;
    int oreForObsidianRobot;
    int clayForObsidianRobot;
    int oreForGeodeRobot;
    int obsidianForGeodeRobot;
} Blueprint;

typedef struct {
    int ore, clay, obsidian, geode;
    int oreRobots, clayRobots, obsidianRobots, geodeRobots;
} State;

static NSMutableDictionary *memo;
static Blueprint blueprint;

static NSString *hashState(State s, int time) {
    return [NSString stringWithFormat:@"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d",
            time, s.ore, s.clay, s.obsidian, s.geode,
            s.oreRobots, s.clayRobots, s.obsidianRobots, s.geodeRobots];
}

static State farm(State s) {
    s.ore += s.oreRobots;
    s.clay += s.clayRobots;
    s.obsidian += s.obsidianRobots;
    s.geode += s.geodeRobots;
    return s;
}

static int calcMostGeodes(State s, int time, int totalTime, int earliestGeode) {
    if (time == totalTime) return s.geode;
    NSString *h = hashState(s, time);
    NSNumber *cached = memo[h];
    if (cached) return [cached intValue];
    if (s.geode == 0 && time > earliestGeode) return 0;

    int most = s.geode;

    if (s.ore >= blueprint.oreForGeodeRobot && s.obsidian >= blueprint.obsidianForGeodeRobot) {
        State cp = s;
        cp = farm(cp);
        cp.ore -= blueprint.oreForGeodeRobot;
        cp.obsidian -= blueprint.obsidianForGeodeRobot;
        cp.geodeRobots++;
        int newEarliest = cp.geodeRobots == 1 ? MIN(earliestGeode, time + 1) : earliestGeode;
        most = MAX(most, calcMostGeodes(cp, time + 1, totalTime, newEarliest));
        memo[h] = @(most);
        return most;
    }

    if (time <= totalTime - 16 && s.oreRobots < blueprint.oreForObsidianRobot * 2 && s.ore >= blueprint.oreForOreRobot) {
        State cp = s;
        cp.ore -= blueprint.oreForOreRobot;
        cp = farm(cp);
        cp.oreRobots++;
        most = MAX(most, calcMostGeodes(cp, time + 1, totalTime, earliestGeode));
    }
    if (time <= totalTime - 8 && s.clayRobots < blueprint.clayForObsidianRobot && s.ore >= blueprint.oreForClayRobot) {
        State cp = s;
        cp.ore -= blueprint.oreForClayRobot;
        cp = farm(cp);
        cp.clayRobots++;
        most = MAX(most, calcMostGeodes(cp, time + 1, totalTime, earliestGeode));
    }
    if (time <= totalTime - 4 && s.obsidianRobots < blueprint.obsidianForGeodeRobot && s.ore >= blueprint.oreForObsidianRobot && s.clay >= blueprint.clayForObsidianRobot) {
        State cp = s;
        cp.ore -= blueprint.oreForObsidianRobot;
        cp.clay -= blueprint.clayForObsidianRobot;
        cp = farm(cp);
        cp.obsidianRobots++;
        most = MAX(most, calcMostGeodes(cp, time + 1, totalTime, earliestGeode));
    }

    State cp = s;
    cp = farm(cp);
    most = MAX(most, calcMostGeodes(cp, time + 1, totalTime, earliestGeode));

    memo[h] = @(most);
    return most;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [txt componentsSeparatedByString:@"\n"];
        int sum = 0;
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSArray *p = [line componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            blueprint.id = [p[1] intValue];
            blueprint.oreForOreRobot = [p[6] intValue];
            blueprint.oreForClayRobot = [p[12] intValue];
            blueprint.oreForObsidianRobot = [p[18] intValue];
            blueprint.clayForObsidianRobot = [p[21] intValue];
            blueprint.oreForGeodeRobot = [p[27] intValue];
            blueprint.obsidianForGeodeRobot = [p[30] intValue];

            memo = [NSMutableDictionary dictionary];
            State st = {0};
            st.oreRobots = 1;
            sum += blueprint.id * calcMostGeodes(st, 0, 24, 24);
        }
        printf("%d\n", sum);
    }
    return 0;
}
