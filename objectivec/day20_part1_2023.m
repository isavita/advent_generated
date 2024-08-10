#import <Foundation/Foundation.h>

typedef NS_ENUM(NSUInteger, PulseType) {
    PulseTypeLow,
    PulseTypeHigh
};

typedef NS_ENUM(NSUInteger, ModuleType) {
    ModuleTypeBroadcaster,
    ModuleTypeFlipFlop,
    ModuleTypeConjunction,
    ModuleTypeUntyped
};

@interface Module : NSObject
@property (nonatomic, assign) ModuleType type;
@property (nonatomic, strong) NSString *name;
@property (nonatomic, strong) NSMutableArray<NSString *> *destinations;
@property (nonatomic, strong) NSMutableDictionary<NSString *, NSNumber *> *inputs;
@property (nonatomic, assign) BOOL state; // For flip-flop modules
@end

@implementation Module
- (instancetype)initWithType:(ModuleType)type name:(NSString *)name destinations:(NSArray<NSString *> *)destinations {
    self = [super init];
    if (self) {
        _type = type;
        _name = name;
        _destinations = [NSMutableArray arrayWithArray:destinations];
        _inputs = [NSMutableDictionary dictionary];
        _state = NO; // Default state is off for flip-flop modules
    }
    return self;
}
@end

@interface Pulse : NSObject
@property (nonatomic, assign) PulseType type;
@property (nonatomic, strong) NSString *source;
@property (nonatomic, strong) NSString *destination;
@end

@implementation Pulse
- (instancetype)initWithType:(PulseType)type source:(NSString *)source destination:(NSString *)destination {
    self = [super init];
    if (self) {
        _type = type;
        _source = source;
        _destination = destination;
    }
    return self;
}
@end

void processPulse(Module *module, PulseType pulseType, NSMutableArray<Pulse *> *pulses, NSMutableDictionary<NSString *, Module *> *modules, NSUInteger *lowPulseCount, NSUInteger *highPulseCount) {
    if (module.type == ModuleTypeBroadcaster) {
        for (NSString *destination in module.destinations) {
            Pulse *pulse = [[Pulse alloc] initWithType:pulseType source:module.name destination:destination];
            [pulses addObject:pulse];
            if (pulseType == PulseTypeLow) {
                (*lowPulseCount)++;
            } else {
                (*highPulseCount)++;
            }
        }
    } else if (module.type == ModuleTypeFlipFlop) {
        if (pulseType == PulseTypeLow) {
            module.state = !module.state;
            PulseType outputPulseType = module.state ? PulseTypeHigh : PulseTypeLow;
            for (NSString *destination in module.destinations) {
                Pulse *pulse = [[Pulse alloc] initWithType:outputPulseType source:module.name destination:destination];
                [pulses addObject:pulse];
                if (outputPulseType == PulseTypeLow) {
                    (*lowPulseCount)++;
                } else {
                    (*highPulseCount)++;
                }
            }
        }
    } else if (module.type == ModuleTypeConjunction) {
        module.inputs[module.name] = @(pulseType);
        BOOL allHigh = YES;
        for (NSNumber *pulse in module.inputs.allValues) {
            if (pulse.integerValue == PulseTypeLow) {
                allHigh = NO;
                break;
            }
        }
        PulseType outputPulseType = allHigh ? PulseTypeLow : PulseTypeHigh;
        for (NSString *destination in module.destinations) {
            Pulse *pulse = [[Pulse alloc] initWithType:outputPulseType source:module.name destination:destination];
            [pulses addObject:pulse];
            if (outputPulseType == PulseTypeLow) {
                (*lowPulseCount)++;
            } else {
                (*highPulseCount)++;
            }
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];

        NSMutableDictionary<NSString *, Module *> *modules = [NSMutableDictionary dictionary];
        NSMutableSet<NSString *> *conjunctionInputs = [NSMutableSet set];

        for (NSString *line in lines) {
            NSArray *components = [line componentsSeparatedByString:@" -> "];
            if (components.count == 2) {
                NSString *moduleInfo = components[0];
                NSArray *destinations = [components[1] componentsSeparatedByString:@", "];

                ModuleType type;
                NSString *name;
                if ([moduleInfo hasPrefix:@"broadcaster"]) {
                    type = ModuleTypeBroadcaster;
                    name = @"broadcaster";
                } else if ([moduleInfo hasPrefix:@"%"]) {
                    type = ModuleTypeFlipFlop;
                    name = [moduleInfo substringFromIndex:1];
                } else if ([moduleInfo hasPrefix:@"&"]) {
                    type = ModuleTypeConjunction;
                    name = [moduleInfo substringFromIndex:1];
                } else {
                    type = ModuleTypeUntyped;
                    name = moduleInfo;
                }

                Module *module = [[Module alloc] initWithType:type name:name destinations:destinations];
                modules[name] = module;

                if (type == ModuleTypeConjunction) {
                    [conjunctionInputs addObjectsFromArray:destinations];
                }
            }
        }

        for (NSString *name in conjunctionInputs) {
            Module *module = modules[name];
            if (module) {
                for (NSString *destination in module.destinations) {
                    Module *destinationModule = modules[destination];
                    if (destinationModule && destinationModule.type == ModuleTypeConjunction) {
                        destinationModule.inputs[name] = @(PulseTypeLow);
                    }
                }
            }
        }

        NSUInteger lowPulseCount = 0;
        NSUInteger highPulseCount = 0;

        for (NSUInteger i = 0; i < 1000; i++) {
            NSMutableArray<Pulse *> *pulses = [NSMutableArray array];
            [pulses addObject:[[Pulse alloc] initWithType:PulseTypeLow source:@"button" destination:@"broadcaster"]];
            lowPulseCount++;

            while (pulses.count > 0) {
                Pulse *pulse = pulses.firstObject;
                [pulses removeObjectAtIndex:0];

                Module *module = modules[pulse.destination];
                if (module) {
                    processPulse(module, pulse.type, pulses, modules, &lowPulseCount, &highPulseCount);
                }
            }
        }

        NSUInteger result = lowPulseCount * highPulseCount;
        NSLog(@"Result: %lu", (unsigned long)result);
    }
    return 0;
}