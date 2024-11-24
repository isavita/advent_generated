
#!/usr/bin/env tclsh

proc readInput {filename} {
    set fp [open $filename r]
    set algorithm [string map {"\n" ""} [gets $fp]]
    gets $fp 
    set image {}
    while {[gets $fp line] != -1} {
        lappend image [split $line ""]
    }
    close $fp
    return [list $algorithm $image]
}

proc calculateIndex {i j image flip} {
    set index 0
    for {set di -1} {$di <= 1} {incr di} {
        for {set dj -1} {$dj <= 1} {incr dj} {
            set index [expr {$index << 1}]
            set ni [expr {$i + $di}]
            set nj [expr {$j + $dj}]
            
            if {$ni >= 0 && $ni < [llength $image] && 
                $nj >= 0 && $nj < [llength [lindex $image 0]]} {
                if {[lindex $image $ni $nj] eq "#"} {
                    incr index
                }
            } elseif {$flip} {
                incr index
            }
        }
    }
    return $index
}

proc applyAlgorithm {image algorithm flip} {
    set height [expr {[llength $image] + 2}]
    set width [expr {[llength [lindex $image 0]] + 2}]
    
    set enhancedImage {}
    for {set i 0} {$i < $height} {incr i} {
        set row {}
        for {set j 0} {$j < $width} {incr j} {
            set index [calculateIndex [expr {$i-1}] [expr {$j-1}] $image $flip]
            lappend row [string index $algorithm $index]
        }
        lappend enhancedImage $row
    }
    return $enhancedImage
}

proc enhanceImage {image algorithm times} {
    for {set i 0} {$i < $times} {incr i} {
        set flip [expr {$i % 2 == 1 && [string index $algorithm 0] eq "#"}]
        set image [applyAlgorithm $image $algorithm $flip]
    }
    return $image
}

proc countLitPixels {image} {
    set count 0
    foreach row $image {
        foreach pixel $row {
            if {$pixel eq "#"} {
                incr count
            }
        }
    }
    return $count
}

# Main execution
set input [readInput "input.txt"]
set algorithm [lindex $input 0]
set image [lindex $input 1]

set enhancedImage [enhanceImage $image $algorithm 2]
puts [countLitPixels $enhancedImage]
