proc decodeImage {input width height} {
    set layerSize [expr {$width * $height}]
    set layers [split $input ""]
    set numLayers [expr {[llength $layers] / $layerSize}]
    
    set minZeroLayer -1
    set minZeroCount 1e9

    for {set i 0} {$i < $numLayers} {incr i} {
        set zeroCount 0
        set oneCount 0
        set twoCount 0
        set layer [lrange $layers [expr {$i * $layerSize}] [expr {($i + 1) * $layerSize - 1}]]

        foreach pixel $layer {
            switch -- $pixel {
                0 {incr zeroCount}
                1 {incr oneCount}
                2 {incr twoCount}
            }
        }
        
        if {$zeroCount < $minZeroCount} {
            set minZeroCount $zeroCount
            set minZeroLayer [list $oneCount $twoCount]
        }
    }
    return [expr {[lindex $minZeroLayer 0] * [lindex $minZeroLayer 1]}]
}

set fileId [open "input.txt" r]
set inputData [read $fileId]
close $fileId

set width 25
set height 6

puts [decodeImage $inputData $width $height]