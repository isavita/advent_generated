#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *allergenIngredients = [NSMutableDictionary dictionary];
        NSMutableDictionary *ingredientCount = [NSMutableDictionary dictionary];
        
        for (NSString *line in lines) {
            NSArray *components = [line componentsSeparatedByString:@" (contains "];
            NSArray *ingredients = [components[0] componentsSeparatedByString:@" "];
            NSArray *allergens = [[components[1] stringByReplacingOccurrencesOfString:@")" withString:@""] componentsSeparatedByString:@", "];
            
            for (NSString *ingredient in ingredients) {
                ingredientCount[ingredient] = @([ingredientCount[ingredient] intValue] + 1);
            }
            
            for (NSString *allergen in allergens) {
                if (allergenIngredients[allergen]) {
                    NSMutableSet *possibleIngredients = [allergenIngredients[allergen] mutableCopy];
                    [possibleIngredients intersectSet:[NSSet setWithArray:ingredients]];
                    allergenIngredients[allergen] = possibleIngredients;
                } else {
                    allergenIngredients[allergen] = [NSSet setWithArray:ingredients];
                }
            }
        }
        
        NSMutableSet *allPossibleIngredients = [NSMutableSet setWithArray:[ingredientCount allKeys]];
        for (NSSet *ingredients in [allergenIngredients allValues]) {
            [allPossibleIngredients minusSet:ingredients];
        }
        
        int count = 0;
        for (NSString *ingredient in allPossibleIngredients) {
            count += [ingredientCount[ingredient] intValue];
        }
        
        printf("%d\n", count);
    }
    return 0;
}