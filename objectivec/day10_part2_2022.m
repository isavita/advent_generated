#import <Foundation/Foundation.h>

int main() {
	@autoreleasepool {
		NSMutableArray *x = [NSMutableArray arrayWithObject:@(1)];
		NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
		NSArray *lines = [content componentsSeparatedByString:@"\n"];
		
		for (NSString *line in lines) {
			if ([line isEqualToString:@"noop"]) {
				[x addObject:[x lastObject]];
			} else {
				int n;
				sscanf([line UTF8String], "addx %d", &n);
				[x addObject:[x lastObject]];
				[x addObject:@([x.lastObject intValue] + n)];
			}
		}
		
		NSMutableDictionary *grid = [NSMutableDictionary dictionary];
		for (int i = 0; i < x.count; i++) {
			int crtx = i % 40;
			int crty = i / 40;
			if (abs(crtx - [x[i] intValue]) <= 1) {
				grid[[NSValue valueWithPoint:NSMakePoint(crtx, crty)]] = [NSNull null];
			} else {
				[grid removeObjectForKey:[NSValue valueWithPoint:NSMakePoint(crtx, crty)]];
			}
		}
		
		for (int y = 0; y < 6; y++) {
			for (int x = 0; x < 40; x++) {
				if (grid[[NSValue valueWithPoint:NSMakePoint(x, y)]] != nil) {
					printf("#");
				} else {
					printf(".");
				}
			}
			printf("\n");
		}
	}
	
	return 0;
}

int abs(int x) {
	return (x < 0) ? -x : x;
}