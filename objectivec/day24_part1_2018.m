
#import <Foundation/Foundation.h>

typedef NS_ENUM(int, ArmyType) { ARMY_IMMUNE = 0, ARMY_INFECTION = 1 };

@interface Group : NSObject
@property ArmyType army;
@property int gid;               // stable id within its army (not required but useful)
@property long long units;
@property long long hp;
@property long long attackDamage;
@property NSString *attackType;
@property int initiative;
@property NSSet<NSString*> *weak;
@property NSSet<NSString*> *immune;
@property (nonatomic, assign) NSInteger targetIndex; // index in global groups array, or -1
@property (nonatomic, assign) BOOL targeted;
@end

@implementation Group
- (long long)effectivePower { return self.units * self.attackDamage; }
@end

static NSString *Trim(NSString *s) {
    return [s stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
}

static NSArray<NSString*> *SplitNonEmpty(NSString *s, NSString *sep) {
    NSArray *parts = [s componentsSeparatedByString:sep];
    NSMutableArray *out = [NSMutableArray arrayWithCapacity:parts.count];
    for (NSString *p in parts) {
        NSString *t = Trim(p);
        if (t.length) [out addObject:t];
    }
    return out;
}

static long long DamageFromTo(Group *att, Group *def) {
    if (att.units <= 0 || def.units <= 0) return 0;
    if ([def.immune containsObject:att.attackType]) return 0;
    long long dmg = att.effectivePower;
    if ([def.weak containsObject:att.attackType]) dmg *= 2;
    return dmg;
}

static Group *ParseGroupLine(NSString *line, ArmyType army, int gid) {
    // Example:
    // 17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
    // Parentheses part optional.
    NSRegularExpression *re = [NSRegularExpression regularExpressionWithPattern:
                              @"^(\\d+) units each with (\\d+) hit points(?: \\(([^\\)]+)\\))? with an attack that does (\\d+) ([a-z]+) damage at initiative (\\d+)$"
                                                                             options:0 error:nil];
    NSTextCheckingResult *m = [re firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
    if (!m) return nil;

    NSString* (^grp)(NSInteger) = ^NSString* (NSInteger i) {
        NSRange r = [m rangeAtIndex:i];
        if (r.location == NSNotFound) return nil;
        return [line substringWithRange:r];
    };

    Group *g = [Group new];
    g.army = army;
    g.gid = gid;
    g.units = [grp(1) longLongValue];
    g.hp = [grp(2) longLongValue];
    NSString *attrs = grp(3); // may be nil
    g.attackDamage = [grp(4) longLongValue];
    g.attackType = grp(5);
    g.initiative = [grp(6) intValue];
    g.targetIndex = -1;
    g.targeted = NO;

    NSMutableSet *weak = [NSMutableSet set];
    NSMutableSet *imm = [NSMutableSet set];

    if (attrs && attrs.length) {
        // attrs like: "weak to radiation, bludgeoning; immune to fire"
        NSArray *clauses = SplitNonEmpty(attrs, @";");
        for (NSString *cl in clauses) {
            NSString *c = Trim(cl);
            if ([c hasPrefix:@"weak to "]) {
                NSString *list = [c substringFromIndex:[@"weak to " length]];
                for (NSString *t in SplitNonEmpty(list, @",")) [weak addObject:t];
            } else if ([c hasPrefix:@"immune to "]) {
                NSString *list = [c substringFromIndex:[@"immune to " length]];
                for (NSString *t in SplitNonEmpty(list, @",")) [imm addObject:t];
            }
        }
    }

    g.weak = weak;
    g.immune = imm;
    return g;
}

static long long Simulate(NSMutableArray<Group*> *groups) {
    while (true) {
        long long immuneUnits = 0, infectUnits = 0;
        for (Group *g in groups) {
            if (g.units > 0) {
                if (g.army == ARMY_IMMUNE) immuneUnits += g.units;
                else infectUnits += g.units;
            }
        }
        if (immuneUnits == 0 || infectUnits == 0) return immuneUnits + infectUnits;

        // reset targeting state
        for (Group *g in groups) { g.targetIndex = -1; g.targeted = NO; }

        // target selection order: decreasing effective power, tie by initiative
        NSArray<Group*> *selectionOrder = [groups sortedArrayUsingComparator:^NSComparisonResult(Group *a, Group *b) {
            long long ea = (a.units>0)? a.effectivePower : -1;
            long long eb = (b.units>0)? b.effectivePower : -1;
            if (ea != eb) return (ea > eb) ? NSOrderedAscending : NSOrderedDescending;
            if (a.initiative != b.initiative) return (a.initiative > b.initiative) ? NSOrderedAscending : NSOrderedDescending;
            return NSOrderedSame;
        }];

        // choose targets
        for (Group *att in selectionOrder) {
            if (att.units <= 0) continue;
            long long bestDmg = 0;
            long long bestEP = 0;
            int bestInit = 0;
            NSInteger bestIdx = -1;

            for (NSInteger i = 0; i < (NSInteger)groups.count; i++) {
                Group *def = groups[i];
                if (def.units <= 0) continue;
                if (def.army == att.army) continue;
                if (def.targeted) continue;

                long long dmg = DamageFromTo(att, def);
                if (dmg <= 0) continue;

                long long dep = def.effectivePower;
                int dini = def.initiative;

                BOOL better = NO;
                if (dmg > bestDmg) better = YES;
                else if (dmg == bestDmg) {
                    if (dep > bestEP) better = YES;
                    else if (dep == bestEP && dini > bestInit) better = YES;
                }
                if (better) {
                    bestDmg = dmg;
                    bestEP = dep;
                    bestInit = dini;
                    bestIdx = i;
                }
            }

            if (bestIdx != -1) {
                att.targetIndex = bestIdx;
                groups[bestIdx].targeted = YES;
            }
        }

        // attack order: decreasing initiative
        NSArray<Group*> *attackOrder = [groups sortedArrayUsingComparator:^NSComparisonResult(Group *a, Group *b) {
            if (a.initiative != b.initiative) return (a.initiative > b.initiative) ? NSOrderedAscending : NSOrderedDescending;
            return NSOrderedSame;
        }];

        BOOL anyKilled = NO;
        for (Group *att in attackOrder) {
            if (att.units <= 0) continue;
            if (att.targetIndex < 0) continue;
            Group *def = groups[att.targetIndex];
            if (def.units <= 0) continue;

            long long dmg = DamageFromTo(att, def);
            if (dmg <= 0) continue;

            long long killed = dmg / def.hp;
            if (killed > def.units) killed = def.units;
            if (killed > 0) anyKilled = YES;
            def.units -= killed;
        }

        // stalemate detection: no units killed in a full round => infinite loop, stop
        if (!anyKilled) {
            long long total = 0;
            for (Group *g in groups) if (g.units > 0) total += g.units;
            return total;
        }

        // remove dead groups (optional; we can keep with units=0, but pruning is cleaner)
        NSIndexSet *dead = [groups indexesOfObjectsPassingTest:^BOOL(Group *g, NSUInteger idx, BOOL *stop) {
            return g.units <= 0;
        }];
        if (dead.count) [groups removeObjectsAtIndexes:dead];
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *err = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&err];
        if (!input) return 1;

        NSArray<NSString*> *lines = [input componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];

        NSMutableArray<Group*> *groups = [NSMutableArray array];
        ArmyType current = ARMY_IMMUNE;
        int gidImm = 0, gidInf = 0;

        for (NSString *raw in lines) {
            NSString *line = Trim(raw);
            if (line.length == 0) continue;
            if ([line isEqualToString:@"Immune System:"]) { current = ARMY_IMMUNE; continue; }
            if ([line isEqualToString:@"Infection:"]) { current = ARMY_INFECTION; continue; }

            int gid = (current == ARMY_IMMUNE) ? (++gidImm) : (++gidInf);
            Group *g = ParseGroupLine(line, current, gid);
            if (!g) return 1;
            [groups addObject:g];
        }

        long long result = Simulate(groups);
        printf("%lld\n", result);
    }
    return 0;
}
