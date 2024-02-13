#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *inputLines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *replacements = [NSMutableDictionary dictionary];
        NSString *medicineMolecule;
        
        for (NSString *line in inputLines) {
            if ([line containsString:@"=>"]) {
                NSArray *parts = [line componentsSeparatedByString:@" => "];
                NSString *key = parts[0];
                NSString *value = parts[1];
                
                NSMutableArray *values = replacements[key];
                if (!values) {
                    values = [NSMutableArray array];
                }
                [values addObject:value];
                replacements[key] = values;
            } else if (line.length > 0) {
                medicineMolecule = line;
            }
        }
        
        NSMutableSet *distinctMolecules = [NSMutableSet set];
        
        for (NSString *key in replacements.allKeys) {
            NSRange range = [medicineMolecule rangeOfString:key options:NSLiteralSearch range:NSMakeRange(0, medicineMolecule.length)];
            while (range.location != NSNotFound) {
                NSArray *values = replacements[key];
                for (NSString *value in values) {
                    NSString *newMolecule = [medicineMolecule stringByReplacingCharactersInRange:range withString:value];
                    [distinctMolecules addObject:newMolecule];
                }
                range = [medicineMolecule rangeOfString:key options:NSLiteralSearch range:NSMakeRange(range.location + 1, medicineMolecule.length - range.location - 1)];
            }
        }
        
        printf("%lu\n", distinctMolecules.count);
    }
    return 0;
}