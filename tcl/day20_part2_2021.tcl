
proc read_input {filename} {
    set file [open $filename r]
    set algorithm [string trim [gets $file]]
    gets $file ;# skip empty line

    set image {}
    while {[gets $file line] >= 0} {
        set row {}
        foreach char [split [string trim $line] ""] {
            lappend row [expr {$char eq "#"}]
        }
        lappend image $row
    }
    close $file
    return [list $algorithm $image]
}

proc enhance_image {algorithm image use_infinite_lit} {
    set expand_by 1
    set new_image {}
    set height [llength $image]
    set width [llength [lindex $image 0]]
    
    for {set y [expr {-$expand_by}]} {$y < [expr {$height + $expand_by}]} {incr y} {
        set new_row_index [expr {$y + $expand_by}]
        set new_row {}
        for {set x [expr {-$expand_by}]} {$x < [expr {$width + $expand_by}]} {incr x} {
            set index 0
            for {set dy -1} {$dy <= 1} {incr dy} {
                for {set dx -1} {$dx <= 1} {incr dx} {
                    set index [expr {($index << 1)}]
                    set ny [expr {$y + $dy}]
                    set nx [expr {$x + $dx}]
                    if {$ny >= 0 && $ny < $height && $nx >= 0 && $nx < $width} {
                        if {[lindex [lindex $image $ny] $nx]} {
                            incr index
                        }
                    } elseif {$use_infinite_lit} {
                        incr index
                    }
                }
            }
            lappend new_row [expr {[string index $algorithm $index] eq "#"}]
        }
        lappend new_image $new_row
    }
    return $new_image
}

proc count_lit_pixels {image} {
    set count 0
    foreach row $image {
        foreach pixel $row {
            if {$pixel} {
                incr count
            }
        }
    }
    return $count
}

proc main {} {
    set iterations 50
    lassign [read_input "input.txt"] algorithm image

    for {set i 0} {$i < $iterations} {incr i} {
        set use_infinite_lit [expr {($i % 2 == 1) && ([string index $algorithm 0] eq "#")}]
        set image [enhance_image $algorithm $image $use_infinite_lit]
    }

    puts [count_lit_pixels $image]
}

main
