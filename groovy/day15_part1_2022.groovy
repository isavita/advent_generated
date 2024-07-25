
import java.nio.file.Files
import java.nio.file.Paths

class BeaconExclusionZone {
    static void main(String[] args) {
        // Read input from the file
        def inputFile = 'input.txt'
        def lines = Files.readAllLines(Paths.get(inputFile))

        // Row to check
        int targetRow = 2000000
        def sensorData = []

        // Parse the input
        lines.each { line ->
            def matcher = (line =~ /Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/)
            if (matcher) {
                int sensorX = Integer.parseInt(matcher[0][1])
                int sensorY = Integer.parseInt(matcher[0][2])
                int beaconX = Integer.parseInt(matcher[0][3])
                int beaconY = Integer.parseInt(matcher[0][4])
                sensorData << [sensorX, sensorY, beaconX, beaconY]
            }
        }

        // Set to track covered positions
        def coveredPositions = new HashSet<Integer>()

        // Calculate coverage for the target row
        sensorData.each { sensor ->
            int sensorX = sensor[0]
            int sensorY = sensor[1]
            int beaconX = sensor[2]
            int beaconY = sensor[3]

            // Calculate Manhattan distance from sensor to beacon
            int distance = Math.abs(sensorX - beaconX) + Math.abs(sensorY - beaconY)

            // Calculate the vertical distance from the sensor to the target row
            int verticalDistance = Math.abs(sensorY - targetRow)

            // If the sensor can cover the target row
            if (verticalDistance <= distance) {
                // Calculate the horizontal coverage range
                int horizontalCoverage = distance - verticalDistance
                int startX = sensorX - horizontalCoverage
                int endX = sensorX + horizontalCoverage

                // Add covered positions to the set
                (startX..endX).each { x ->
                    coveredPositions.add(x)
                }
            }
        }

        // Remove positions occupied by beacons in the target row
        sensorData.each { sensor ->
            int beaconX = sensor[2]
            int beaconY = sensor[3]
            if (beaconY == targetRow) {
                coveredPositions.remove(beaconX)
            }
        }

        // Output the count of positions that cannot contain a beacon
        println "Positions that cannot contain a beacon in row $targetRow: ${coveredPositions.size()}"
    }
}
