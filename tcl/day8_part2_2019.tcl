
# Read the input from the file
set fileId [open "input.txt" r]
set imageData [read $fileId]
close $fileId

# Constants for the image dimensions
set width 25
set height 6
set layerSize [expr {$width * $height}]

# Split the image data into layers
set layers [split $imageData ""]

# Part 1: Find the layer with the fewest 0 digits and calculate the result
set minZeroCount [expr {$layerSize + 1}]
set result 0

foreach {layer} [split $imageData ""] {
    set zeroCount 0
    set oneCount 0
    set twoCount 0

    for {set i 0} {$i < $layerSize} {incr i} {
        set digit [string index $imageData [expr {$i + [string length $layer] * $layer}]]
        if {$digit == 0} {
            incr zeroCount
        } elseif {$digit == 1} {
            incr oneCount
        } elseif {$digit == 2} {
            incr twoCount
        }
    }

    if {$zeroCount < $minZeroCount} {
        set minZeroCount $zeroCount
        set result [expr {$oneCount * $twoCount}]
    }
}

puts "Part 1 Result: $result"

# Part 2: Decode the image
set decodedImage [string repeat "2" $layerSize]

for {set i 0} {$i < $layerSize} {incr i} {
    for {set layer 0} {$layer < [expr {[string length $imageData] / $layerSize}]} {incr layer} {
        set digit [string index $imageData [expr {$i + $layer * $layerSize}]]
        if {$digit != 2} {
            set decodedImage [string replace $decodedImage $i $i $digit]
            break
        }
    }
}

# Print the decoded image
puts "Part 2 Decoded Image:"
for {set y 0} {$y < $height} {incr y} {
    set row [string range $decodedImage [expr {$y * $width}] [expr {($y + 1) * $width - 1}]]
    regsub -all "0" $row " " row
    regsub -all "1" $row "#" row
    puts $row
}
