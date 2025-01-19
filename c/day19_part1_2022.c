
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_BLUEPRINTS 30

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
    Blueprint blueprint;
    int ore;
    int clay;
    int obsidian;
    int geode;
    int oreRobots;
    int clayRobots;
    int obsidianRobots;
    int geodeRobots;
} State;

State newState(Blueprint blueprint) {
    State s;
    s.blueprint = blueprint;
    s.ore = 0;
    s.clay = 0;
    s.obsidian = 0;
    s.geode = 0;
    s.oreRobots = 1;
    s.clayRobots = 0;
    s.obsidianRobots = 0;
    s.geodeRobots = 0;
    return s;
}

void farm(State *s) {
    s->ore += s->oreRobots;
    s->clay += s->clayRobots;
    s->obsidian += s->obsidianRobots;
    s->geode += s->geodeRobots;
}

unsigned long long hash(const State *s, int time) {
    unsigned long long h = time;
    h = h * 31 + s->ore;
    h = h * 31 + s->clay;
    h = h * 31 + s->obsidian;
    h = h * 31 + s->geode;
    h = h * 31 + s->oreRobots;
    h = h * 31 + s->clayRobots;
    h = h * 31 + s->obsidianRobots;
    h = h * 31 + s->geodeRobots;
    return h;
}

State copyState(const State *s) {
    return *s;
}

int maxInt(int a, int b) {
    return a > b ? a : b;
}

int minInt(int a, int b) {
    return a < b ? a : b;
}


int calcMostGeodes(State s, int time, int totalTime, int earliestGeode, int *memo, int memoSize) {
  if (time == totalTime) {
        return s.geode;
    }

    unsigned long long h = hash(&s, time);
    int memoIndex = h % memoSize;
    if(memo[memoIndex] != INT_MIN){
      if ((memo[memoIndex] >> 10) == h){
        return (memo[memoIndex] & 1023);
      }
    }

    if (s.geode == 0 && time > earliestGeode) {
        return 0;
    }


    int mostGeodes = s.geode;


    if (s.ore >= s.blueprint.oreForGeodeRobot &&
        s.obsidian >= s.blueprint.obsidianForGeodeRobot) {
        State cp = copyState(&s);

        farm(&cp);

        cp.ore -= cp.blueprint.oreForGeodeRobot;
        cp.obsidian -= cp.blueprint.obsidianForGeodeRobot;
        cp.geodeRobots++;
        if (cp.geodeRobots == 1) {
            earliestGeode = minInt(earliestGeode, time + 1);
        }
        mostGeodes = maxInt(mostGeodes, calcMostGeodes(cp, time + 1, totalTime, earliestGeode, memo, memoSize));

    } else {

      if (time <= totalTime - 16 &&
          s.oreRobots < s.blueprint.oreForObsidianRobot * 2 &&
          s.ore >= s.blueprint.oreForOreRobot) {
            State cp = copyState(&s);
            cp.ore -= cp.blueprint.oreForOreRobot;
            farm(&cp);
            cp.oreRobots++;
           mostGeodes = maxInt(mostGeodes, calcMostGeodes(cp, time + 1, totalTime, earliestGeode, memo, memoSize));
        }
      if (time <= totalTime - 8 &&
            s.clayRobots < s.blueprint.clayForObsidianRobot &&
          s.ore >= s.blueprint.oreForClayRobot) {
              State cp = copyState(&s);
              cp.ore -= cp.blueprint.oreForClayRobot;
                farm(&cp);
              cp.clayRobots++;
              mostGeodes = maxInt(mostGeodes, calcMostGeodes(cp, time + 1, totalTime, earliestGeode, memo, memoSize));
        }
      if (time <= totalTime - 4 &&
            s.obsidianRobots < s.blueprint.obsidianForGeodeRobot &&
          s.ore >= s.blueprint.oreForObsidianRobot && s.clay >= s.blueprint.clayForObsidianRobot) {
             State cp = copyState(&s);
             cp.ore -= cp.blueprint.oreForObsidianRobot;
              cp.clay -= cp.blueprint.clayForObsidianRobot;
              farm(&cp);
            cp.obsidianRobots++;
              mostGeodes = maxInt(mostGeodes, calcMostGeodes(cp, time + 1, totalTime, earliestGeode, memo, memoSize));
        }

        State cp = copyState(&s);
         farm(&cp);
        mostGeodes = maxInt(mostGeodes, calcMostGeodes(cp, time + 1, totalTime, earliestGeode, memo, memoSize));

    }
    memo[memoIndex] =  ((h << 10) | mostGeodes);
    return mostGeodes;

}

int part1(Blueprint blueprints[], int numBlueprints) {
    int sum = 0;
    int memoSize = 65536;
     int *memo = (int*) malloc(memoSize * sizeof(int));
     for(int i=0; i<memoSize; i++){
       memo[i] = INT_MIN;
     }
    for (int i = 0; i < numBlueprints; i++) {
        State st = newState(blueprints[i]);
       for(int j=0; j<memoSize; j++){
           memo[j] = INT_MIN;
       }
        int geodesMade = calcMostGeodes(st, 0, 24, 24, memo, memoSize);
        sum += st.blueprint.id * geodesMade;
    }
      free(memo);
    return sum;
}

int main() {
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    Blueprint blueprints[MAX_BLUEPRINTS];
    int numBlueprints = 0;

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    while ((read = getline(&line, &len, fp)) != -1) {
        Blueprint bp;
        sscanf(line, "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian.",
               &bp.id, &bp.oreForOreRobot, &bp.oreForClayRobot, &bp.oreForObsidianRobot,
               &bp.clayForObsidianRobot, &bp.oreForGeodeRobot, &bp.obsidianForGeodeRobot);
        blueprints[numBlueprints++] = bp;

    }
    
    fclose(fp);
    if (line) free(line);

    printf("%d\n", part1(blueprints, numBlueprints));

    return 0;
}
