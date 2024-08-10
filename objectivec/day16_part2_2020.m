#import <Foundation/Foundation.h>

@interface Rule : NSObject
@property (nonatomic, strong) NSString *name;
@property (nonatomic, strong) NSArray<NSArray<NSNumber *> *> *ranges;
- (BOOL)isValid:(NSInteger)value;
@end

@implementation Rule
- (BOOL)isValid:(NSInteger)value {
    for (NSArray<NSNumber *> *rng in self.ranges) {
        if (value >= [rng[0] integerValue] && value <= [rng[1] integerValue]) {
            return YES;
        }
    }
    return NO;
}
@end

NSArray<NSNumber *> *parseTicket(NSString *line) {
    NSArray<NSString *> *values = [line componentsSeparatedByString:@","];
    NSMutableArray<NSNumber *> *ticket = [NSMutableArray array];
    for (NSString *value in values) {
        [ticket addObject:@(value.integerValue)];
    }
    return ticket;
}

BOOL isValidTicket(NSArray<NSNumber *> *ticket, NSArray<Rule *> *rules) {
    for (NSNumber *value in ticket) {
        BOOL valid = NO;
        for (Rule *rule in rules) {
            if ([rule isValid:value.integerValue]) {
                valid = YES;
                break;
            }
        }
        if (!valid) return NO;
    }
    return YES;
}

NSDictionary<NSString *, NSNumber *> *solveFieldPositions(NSArray<Rule *> *rules, NSArray<NSArray<NSNumber *> *> *tickets) {
    NSMutableDictionary<NSString *, NSMutableSet<NSNumber *> *> *validPositions = [NSMutableDictionary dictionary];
    for (Rule *rule in rules) {
        validPositions[rule.name] = [NSMutableSet set];
        for (NSInteger i = 0; i < tickets[0].count; i++) {
            BOOL valid = YES;
            for (NSArray<NSNumber *> *ticket in tickets) {
                if (![rule isValid:ticket[i].integerValue]) {
                    valid = NO;
                    break;
                }
            }
            if (valid) [validPositions[rule.name] addObject:@(i)];
        }
    }

    NSMutableDictionary<NSString *, NSNumber *> *fieldPositions = [NSMutableDictionary dictionary];
    while (fieldPositions.count < rules.count) {
        for (NSString *name in validPositions.allKeys) {
            if (validPositions[name].count == 1) {
                NSNumber *pos = validPositions[name].anyObject;
                fieldPositions[name] = pos;
                [validPositions removeObjectForKey:name];
                for (NSMutableSet *positions in validPositions.allValues) {
                    [positions removeObject:pos];
                }
            }
        }
    }
    return fieldPositions;
}

NSInteger calculateDepartureProduct(NSArray<NSNumber *> *ticket, NSDictionary<NSString *, NSNumber *> *fieldPositions) {
    NSInteger product = 1;
    for (NSString *name in fieldPositions) {
        if ([name hasPrefix:@"departure"]) {
            product *= ticket[fieldPositions[name].integerValue].integerValue;
        }
    }
    return product;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) return 1;

        NSArray<NSString *> *lines = [input componentsSeparatedByString:@"\n"];
        NSMutableArray<Rule *> *rules = [NSMutableArray array];
        NSArray<NSNumber *> *myTicket;
        NSMutableArray<NSArray<NSNumber *> *> *nearbyTickets = [NSMutableArray array];
        NSInteger section = 0;

        NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"^([^:]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$" options:0 error:nil];

        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                section++;
                continue;
            }
            switch (section) {
                case 0: {
                    NSTextCheckingResult *match = [regex firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
                    if (match) {
                        Rule *rule = [[Rule alloc] init];
                        rule.name = [line substringWithRange:[match rangeAtIndex:1]];
                        rule.ranges = @[
                            @[@([[line substringWithRange:[match rangeAtIndex:2]] integerValue]), @([[line substringWithRange:[match rangeAtIndex:3]] integerValue])],
                            @[@([[line substringWithRange:[match rangeAtIndex:4]] integerValue]), @([[line substringWithRange:[match rangeAtIndex:5]] integerValue])]
                        ];
                        [rules addObject:rule];
                    }
                    break;
                }
                case 1:
                    if (![line isEqualToString:@"your ticket:"]) {
                        myTicket = parseTicket(line);
                    }
                    break;
                case 2:
                    if (![line isEqualToString:@"nearby tickets:"]) {
                        NSArray<NSNumber *> *ticket = parseTicket(line);
                        if (isValidTicket(ticket, rules)) {
                            [nearbyTickets addObject:ticket];
                        }
                    }
                    break;
            }
        }

        NSDictionary<NSString *, NSNumber *> *fieldPositions = solveFieldPositions(rules, nearbyTickets);
        NSInteger departureProduct = calculateDepartureProduct(myTicket, fieldPositions);
        NSLog(@"%ld", (long)departureProduct);
    }
    return 0;
}