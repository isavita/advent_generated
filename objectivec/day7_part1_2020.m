
#import <Foundation/Foundation.h>

#define MAX_RULES 600
#define MAX_CONTAINS 10

typedef struct {
    char name[30];
    int contains[MAX_CONTAINS];
    int count;
} Rule;

static Rule rules[MAX_RULES];
static int ruleCount = 0;
static int shinyId = -1;
static char memo[MAX_RULES];

static int idForColor(NSString *color) {
    const char *c = [color UTF8String];
    for (int i = 0; i < ruleCount; i++)
        if (strcmp(rules[i].name, c) == 0) return i;
    strcpy(rules[ruleCount].name, c);
    rules[ruleCount].count = 0;
    if ([color isEqualToString:@"shiny gold"]) shinyId = ruleCount;
    return ruleCount++;
}

static BOOL canHoldShiny(int bag) {
    if (bag == shinyId) return YES;
    if (memo[bag] != -1) return memo[bag];
    for (int i = 0; i < rules[bag].count; i++)
        if (canHoldShiny(rules[bag].contains[i])) return !!(memo[bag] = 1);
    return !!(memo[bag] = 0);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                               encoding:NSUTF8StringEncoding
                                                  error:nil];
        for (NSString *line in [txt componentsSeparatedByString:@"\n"]) {
            NSArray *p = [line componentsSeparatedByString:@" bags contain "];
            if (p.count < 2) continue;
            NSString *outer = p[0];
            int outerId = idForColor(outer);
            NSString *inners = [[[p[1] stringByReplacingOccurrencesOfString:@"." withString:@""]
                                stringByReplacingOccurrencesOfString:@" bags" withString:@""]
                               stringByReplacingOccurrencesOfString:@" bag" withString:@""];
            if ([inners isEqualToString:@"no other"]) continue;
            for (NSString *chunk in [inners componentsSeparatedByString:@", "]) {
                NSArray *w = [chunk componentsSeparatedByString:@" "];
                NSString *color = [NSString stringWithFormat:@"%@ %@", w[1], w[2]];
                int innerId = idForColor(color);
                Rule *r = &rules[outerId];
                if (r->count < MAX_CONTAINS) r->contains[r->count++] = innerId;
            }
        }
        if (shinyId < 0) { printf("0\n"); return 0; }
        memset(memo, -1, sizeof(memo));
        int total = 0;
        for (int i = 0; i < ruleCount; i++)
            if (i != shinyId && canHoldShiny(i)) total++;
        printf("%d\n", total);
    }
    return 0;
}
