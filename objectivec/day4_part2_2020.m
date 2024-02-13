#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *passports = [input componentsSeparatedByString:@"\n\n"];
        
        int validPassports = 0;
        
        for (NSString *passport in passports) {
            NSArray *fields = [passport componentsSeparatedByCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:@" \n"]];
            NSMutableDictionary *passportDict = [NSMutableDictionary dictionary];
            
            for (NSString *field in fields) {
                NSArray *parts = [field componentsSeparatedByString:@":"];
                passportDict[parts[0]] = parts[1];
            }
            
            if ([passportDict count] == 8 || ([passportDict count] == 7 && passportDict[@"cid"] == nil)) {
                BOOL valid = YES;
                
                for (NSString *key in passportDict) {
                    NSString *value = passportDict[key];
                    
                    if ([key isEqualToString:@"byr"]) {
                        int byr = [value intValue];
                        if (byr < 1920 || byr > 2002) {
                            valid = NO;
                            break;
                        }
                    } else if ([key isEqualToString:@"iyr"]) {
                        int iyr = [value intValue];
                        if (iyr < 2010 || iyr > 2020) {
                            valid = NO;
                            break;
                        }
                    } else if ([key isEqualToString:@"eyr"]) {
                        int eyr = [value intValue];
                        if (eyr < 2020 || eyr > 2030) {
                            valid = NO;
                            break;
                        }
                    } else if ([key isEqualToString:@"hgt"]) {
                        if ([value hasSuffix:@"cm"]) {
                            int hgt = [[value substringToIndex:value.length - 2] intValue];
                            if (hgt < 150 || hgt > 193) {
                                valid = NO;
                                break;
                            }
                        } else if ([value hasSuffix:@"in"]) {
                            int hgt = [[value substringToIndex:value.length - 2] intValue];
                            if (hgt < 59 || hgt > 76) {
                                valid = NO;
                                break;
                            }
                        } else {
                            valid = NO;
                            break;
                        }
                    } else if ([key isEqualToString:@"hcl"]) {
                        NSPredicate *hclPredicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", @"^#[0-9a-f]{6}$"];
                        if (![hclPredicate evaluateWithObject:value]) {
                            valid = NO;
                            break;
                        }
                    } else if ([key isEqualToString:@"ecl"]) {
                        NSArray *validEcls = @[@"amb", @"blu", @"brn", @"gry", @"grn", @"hzl", @"oth"];
                        if (![validEcls containsObject:value]) {
                            valid = NO;
                            break;
                        }
                    } else if ([key isEqualToString:@"pid"]) {
                        NSPredicate *pidPredicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", @"^[0-9]{9}$"];
                        if (![pidPredicate evaluateWithObject:value]) {
                            valid = NO;
                            break;
                        }
                    }
                }
                
                if (valid) {
                    validPassports++;
                }
            }
        }
        
        printf("%d\n", validPassports);
    }
    return 0;
}