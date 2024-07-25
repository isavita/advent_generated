
def readComponents(fileName) {
    def components = []
    new File(fileName).eachLine { line ->
        def ports = line.split('/').collect { it.toInteger() }
        components << ports
    }
    return components
}

def findStrongestBridge(components, currentPort, usedComponents) {
    def strongest = 0
    def possibleBridges = []

    components.eachWithIndex { component, index ->
        if (!usedComponents.contains(index) && (component[0] == currentPort || component[1] == currentPort)) {
            // Mark this component as used
            usedComponents.add(index)
            // Calculate the next port
            def nextPort = (component[0] == currentPort) ? component[1] : component[0]
            // Calculate the strength of the current bridge
            def strength = component.sum() + findStrongestBridge(components, nextPort, usedComponents)
            // Update the strongest bridge found
            strongest = Math.max(strongest, strength)
            // Unmark this component for the next iteration
            usedComponents.remove(index)
        }
    }

    return strongest
}

def main() {
    def components = readComponents('input.txt')
    def strongestBridgeStrength = findStrongestBridge(components, 0, [] as Set)
    println "The strength of the strongest bridge is: $strongestBridgeStrength"
}

main()
