
def input = new File("input.txt").text

def findMarkerPosition(String input, int markerLength) {
    for (int i = 0; i < input.size() - markerLength; i++) {
        def marker = input.substring(i, i + markerLength)
        if (marker.toSet().size() == markerLength) {
            return i + markerLength
        }
    }
    return -1
}

def packetMarkerPosition = findMarkerPosition(input, 4)
def messageMarkerPosition = findMarkerPosition(input, 14)

println packetMarkerPosition
println messageMarkerPosition
