
import std.stdio, std.file, std.string, std.conv, std.algorithm, std.range;

struct Component {
    long quantity;
    string chemical;
}

struct Reaction {
    Component[] inputs;
    Component output;
}

Reaction[string] reactions;

long calculateOreNeeded(long fuelAmount) {
    long[string] stock;
    long oreNeeded = 0;

    void produce(string chemical, long quantity) {
        if (chemical == "ORE") {
            oreNeeded += quantity;
            return;
        }

        long inStock = stock.get(chemical, 0);
        if (inStock >= quantity) {
            stock[chemical] = inStock - quantity;
            return;
        }

        long needed = quantity - inStock;
        stock[chemical] = 0;

        auto reaction = reactions[chemical];
        long reactionTimes = (needed + reaction.output.quantity - 1) / reaction.output.quantity;

        foreach (input; reaction.inputs) {
            produce(input.chemical, input.quantity * reactionTimes);
        }

        stock[chemical] += reaction.output.quantity * reactionTimes - needed;
    }

    produce("FUEL", fuelAmount);
    return oreNeeded;
}

void main() {
    foreach (line; readText("input.txt").strip.split("\n")) {
        auto parts = line.split(" => ");
        auto outputParts = parts[1].split(" ");
        auto output = Component(outputParts[0].to!long, outputParts[1]);

        Component[] inputs;
        foreach (inputStr; parts[0].split(", ")) {
            auto inputParts = inputStr.split(" ");
            inputs ~= Component(inputParts[0].to!long, inputParts[1]);
        }
        reactions[output.chemical] = Reaction(inputs, output);
    }

    long oreForOneFuel = calculateOreNeeded(1);
    writeln(oreForOneFuel);

    long trillion = 1_000_000_000_000L;
    long lowerBound = trillion / oreForOneFuel;
    long upperBound = lowerBound * 2;

    while (lowerBound < upperBound - 1) {
        long middle = lowerBound + (upperBound - lowerBound) / 2;
        if (calculateOreNeeded(middle) > trillion) {
            upperBound = middle;
        } else {
            lowerBound = middle;
        }
    }
    writeln(lowerBound);
}
