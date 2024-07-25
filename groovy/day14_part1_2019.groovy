
import groovy.json.JsonSlurper

class Chemical {
    String name
    long quantity

    Chemical(String name, long quantity) {
        this.name = name
        this.quantity = quantity
    }
}

class Reaction {
    List<Chemical> inputs
    Chemical output

    Reaction(List<Chemical> inputs, Chemical output) {
        this.inputs = inputs
        this.output = output
    }
}

class Nanofactory {
    Map<String, Reaction> reactions = [:]
    Map<String, Long> surplus = [:]

    void addReaction(String line) {
        def parts = line.split(" => ")
        def inputs = parts[0].split(", ").collect { parseChemical(it) }
        def output = parseChemical(parts[1])
        reactions[output.name] = new Reaction(inputs, output)
    }

    Chemical parseChemical(String str) {
        def parts = str.split(" ")
        return new Chemical(parts[1], Long.parseLong(parts[0]))
    }

    long calculateOREForFuel(long fuelAmount) {
        return calculateORE("FUEL", fuelAmount)
    }

    long calculateORE(String chemical, long amountNeeded) {
        if (chemical == "ORE") return amountNeeded

        if (surplus.containsKey(chemical)) {
            long surplusAmount = surplus[chemical]
            if (surplusAmount >= amountNeeded) {
                surplus[chemical] -= amountNeeded
                return 0
            } else {
                amountNeeded -= surplusAmount
                surplus.remove(chemical)
            }
        }

        Reaction reaction = reactions[chemical]
        long times = (amountNeeded + reaction.output.quantity - 1) / reaction.output.quantity

        long oreNeeded = 0
        for (Chemical input : reaction.inputs) {
            oreNeeded += calculateORE(input.name, input.quantity * times)
        }

        long produced = reaction.output.quantity * times
        long leftover = produced - amountNeeded
        if (leftover > 0) {
            surplus[chemical] = surplus.getOrDefault(chemical, 0L) + leftover
        }

        return oreNeeded
    }
}

def main() {
    def nanofactory = new Nanofactory()
    new File('input.txt').eachLine { line ->
        nanofactory.addReaction(line)
    }

    long oreRequired = nanofactory.calculateOREForFuel(1)
    println "Minimum ORE required to produce 1 FUEL: $oreRequired"
}

main()
