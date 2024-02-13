#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"%@", error.localizedDescription);
            return 1;
        }

        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSMutableDictionary *allergenMap = [NSMutableDictionary dictionary];
        NSMutableDictionary *ingredientAllergen = [NSMutableDictionary dictionary];

        for (NSString *line in lines) {
            NSArray *parts = [line componentsSeparatedByString:@" (contains "];
            NSArray *ingredients = [parts[0] componentsSeparatedByString:@" "];
            NSArray *allergens = @[];
            if (parts.count > 1) {
                allergens = [[parts[1] substringToIndex:[parts[1] length]-1] componentsSeparatedByString:@", "];
            }

            for (NSString *allergen in allergens) {
                if (!allergenMap[allergen]) {
                    allergenMap[allergen] = [NSMutableDictionary dictionary];
                    for (NSString *ingredient in ingredients) {
                        allergenMap[allergen][ingredient] = @YES;
                    }
                } else {
                    NSMutableDictionary *currentAllergenMap = allergenMap[allergen];
                    for (NSString *ingredient in currentAllergenMap.allKeys) {
                        if (![ingredients containsObject:ingredient]) {
                            [currentAllergenMap removeObjectForKey:ingredient];
                        }
                    }
                }
            }
        }

        while ([allergenMap count] > 0) {
            for (NSString *allergen in [allergenMap allKeys]) {
                if ([allergenMap[allergen] count] == 1) {
                    NSString *ingredient = [allergenMap[allergen] allKeys][0];
                    ingredientAllergen[allergen] = ingredient;
                    for (NSString *key in [allergenMap allKeys]) {
                        [allergenMap[key] removeObjectForKey:ingredient];
                    }
                    [allergenMap removeObjectForKey:allergen];
                }
            }
        }

        NSArray *allergens = [[ingredientAllergen allKeys] sortedArrayUsingSelector:@selector(compare:)];
        NSMutableArray *result = [NSMutableArray array];
        for (NSString *allergen in allergens) {
            [result addObject:ingredientAllergen[allergen]];
        }

        printf("%s\n", [[result componentsJoinedByString:@","] UTF8String]);
    }
    return 0;
}