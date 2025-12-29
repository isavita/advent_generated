
#import <Foundation/Foundation.h>

/**
 * Group class represents a collection of identical units within an army.
 */
@interface Group : NSObject <NSCopying>
@property (nonatomic) NSInteger units;
@property (nonatomic) NSInteger hp;
@property (nonatomic) NSInteger attackDamage;
@property (nonatomic, copy) NSString *attackType;
@property (nonatomic) NSInteger initiative;
@property (nonatomic, strong) NSSet<NSString *> *weaknesses;
@property (nonatomic, strong) NSSet<NSString *> *immunities;
@property (nonatomic) BOOL isInfection;

- (NSInteger)effectivePower;
- (NSInteger)calculateDamageTo:(Group *)target;
@end

@implementation Group

- (NSInteger)effectivePower {
    return self.units * self.attackDamage;
}

- (NSInteger)calculateDamageTo:(Group *)target {
    if ([target.immunities containsObject:self.attackType]) return 0;
    NSInteger dmg = [self effectivePower];
    if ([target.weaknesses containsObject:self.attackType]) dmg *= 2;
    return dmg;
}

- (id)copyWithZone:(NSZone *)zone {
    Group *copy = [[[self class] allocWithZone:zone] init];
    copy.units = self.units;
    copy.hp = self.hp;
    copy.attackDamage = self.attackDamage;
    copy.attackType = [self.attackType copy];
    copy.initiative = self.initiative;
    copy.weaknesses = [self.weaknesses copy];
    copy.immunities = [self.immunities copy];
    copy.isInfection = self.isInfection;
    return copy;
}

@end

// Helper to parse strings like "weak to fire, cold; immune to slashing"
void parseTraits(NSString *traits, Group *g) {
    NSArray *parts = [traits componentsSeparatedByString:@"; "];
    for (NSString *part in parts) {
        if ([part hasPrefix:@"weak to "]) {
            NSString *list = [part substringFromIndex:8];
            g.weaknesses = [NSSet setWithArray:[list componentsSeparatedByString:@", "]];
        } else if ([part hasPrefix:@"immune to "]) {
            NSString *list = [part substringFromIndex:10];
            g.immunities = [NSSet setWithArray:[list componentsSeparatedByString:@", "]];
        }
    }
}

// Parses a single line from the input file into a Group object
Group* parseLine(NSString *line, BOOL isInfection) {
    if ([line length] == 0 || [line hasSuffix:@":"]) return nil;
    
    Group *g = [[Group alloc] init];
    g.isInfection = isInfection;
    g.weaknesses = [NSSet set];
    g.immunities = [NSSet set];
    
    NSScanner *scanner = [NSScanner scannerWithString:line];
    NSInteger units, hp, damage, initiative;
    
    if (![scanner scanInteger:&units]) return nil;
    [scanner scanString:@"units each with" intoString:NULL];
    [scanner scanInteger:&hp];
    [scanner scanString:@"hit points" intoString:NULL];
    g.units = units;
    g.hp = hp;
    
    if ([scanner scanString:@"(" intoString:NULL]) {
        NSString *traits;
        [scanner scanUpToString:@")" intoString:&traits];
        [scanner scanString:@")" intoString:NULL];
        parseTraits(traits, g);
    }
    
    [scanner scanString:@"with an attack that does" intoString:NULL];
    [scanner scanInteger:&damage];
    g.attackDamage = damage;
    
    NSString *type;
    [scanner scanCharactersFromSet:[NSCharacterSet whitespaceCharacterSet] intoString:NULL];
    [scanner scanCharactersFromSet:[NSCharacterSet letterCharacterSet] intoString:&type];
    g.attackType = type;
    
    [scanner scanString:@"damage at initiative" intoString:NULL];
    [scanner scanInteger:&initiative];
    g.initiative = initiative;
    
    return g;
}

/**
 * Runs the battle simulation given a set of groups and an attack boost for the immune system.
 * Returns a dictionary containing the winner's name and total remaining units.
 */
NSDictionary* simulate(NSArray<Group *> *originalGroups, NSInteger boost) {
    NSMutableArray<Group *> *groups = [NSMutableArray array];
    for (Group *g in originalGroups) {
        Group *copy = [g copy];
        if (!copy.isInfection) copy.attackDamage += boost;
        [groups addObject:copy];
    }
    
    while (YES) {
        // Target Selection Phase
        NSArray *selectionOrder = [groups sortedArrayUsingComparator:^NSComparisonResult(Group *a, Group *b) {
            if (a.effectivePower != b.effectivePower) 
                return [@(b.effectivePower) compare:@(a.effectivePower)];
            return [@(b.initiative) compare:@(a.initiative)];
        }];
        
        NSMapTable *targets = [NSMapTable strongToStrongObjectsMapTable];
        NSMutableSet *chosenTargets = [NSMutableSet set];
        
        for (Group *attacker in selectionOrder) {
            if (attacker.units <= 0) continue;
            Group *bestTarget = nil;
            NSInteger maxDmg = 0;
            
            for (Group *defender in groups) {
                if (defender.units <= 0 || attacker.isInfection == defender.isInfection || [chosenTargets containsObject:defender]) continue;
                
                NSInteger dmg = [attacker calculateDamageTo:defender];
                if (dmg == 0) continue;
                
                if (dmg > maxDmg) {
                    maxDmg = dmg;
                    bestTarget = defender;
                } else if (dmg == maxDmg && bestTarget) {
                    if (defender.effectivePower > bestTarget.effectivePower) {
                        bestTarget = defender;
                    } else if (defender.effectivePower == bestTarget.effectivePower) {
                        if (defender.initiative > bestTarget.initiative) {
                            bestTarget = defender;
                        }
                    }
                }
            }
            if (bestTarget) {
                [targets setObject:bestTarget forKey:attacker];
                [chosenTargets addObject:bestTarget];
            }
        }
        
        // Attacking Phase
        NSArray *attackOrder = [groups sortedArrayUsingComparator:^NSComparisonResult(Group *a, Group *b) {
            return [@(b.initiative) compare:@(a.initiative)];
        }];
        
        NSInteger totalKilledThisRound = 0;
        for (Group *attacker in attackOrder) {
            if (attacker.units <= 0) continue;
            Group *defender = [targets objectForKey:attacker];
            if (defender && defender.units > 0) {
                NSInteger dmg = [attacker calculateDamageTo:defender];
                NSInteger killed = dmg / defender.hp;
                if (killed > defender.units) killed = defender.units;
                defender.units -= killed;
                totalKilledThisRound += killed;
            }
        }
        
        NSInteger immuneUnits = 0, infectionUnits = 0;
        for (Group *g in groups) {
            if (g.units > 0) {
                if (g.isInfection) infectionUnits += g.units;
                else immuneUnits += g.units;
            }
        }
        
        // Return if one side is wiped out
        if (immuneUnits == 0 || infectionUnits == 0) {
            return @{@"winner": (immuneUnits > 0 ? @"Immune" : @"Infection"), @"units": @(immuneUnits + infectionUnits)};
        }
        // Stalemate detection (no units were killed in a round)
        if (totalKilledThisRound == 0) {
            return @{@"winner": @"None", @"units": @(immuneUnits + infectionUnits)};
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        if (!content) {
            fprintf(stderr, "Could not read input.txt\n");
            return 1;
        }
        
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        NSMutableArray *groups = [NSMutableArray array];
        BOOL isInfectionSection = NO;
        for (NSString *line in lines) {
            NSString *trimmed = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if ([trimmed isEqualToString:@"Immune System:"]) isInfectionSection = NO;
            else if ([trimmed isEqualToString:@"Infection:"]) isInfectionSection = YES;
            else {
                Group *g = parseLine(trimmed, isInfectionSection);
                if (g) [groups addObject:g];
            }
        }
        
        // Part 1
        NSDictionary *p1 = simulate(groups, 0);
        printf("Part 1: %ld\n", [[p1 objectForKey:@"units"] longValue]);
        
        // Part 2: Smallest boost where Immune System wins (Binary Search)
        NSInteger low = 1, high = 100000, bestUnits = 0;
        while (low <= high) {
            NSInteger mid = low + (high - low) / 2;
            NSDictionary *res = simulate(groups, mid);
            if ([res[@"winner"] isEqualToString:@"Immune"]) {
                bestUnits = [[res objectForKey:@"units"] longValue];
                high = mid - 1;
            } else {
                low = mid + 1;
            }
        }
        printf("Part 2: %ld\n", (long)bestUnits);
    }
    return 0;
}

