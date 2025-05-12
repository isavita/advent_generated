
proc parseInput {input} {
    set cubes {}
    foreach line [split [string trim $input] "\n"] {
        set parts [split $line " "]
        set state [lindex $parts 0]
        set state_int [expr {$state eq "on" ? 1 : 0}]
        set ranges [lindex $parts 1]
        if {[regexp {x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)} $ranges \
             _ x1 x2 y1 y2 z1 z2]} {
            lappend cubes [list $state_int $x1 $x2 $y1 $y2 $z1 $z2]
        }
    }
    return $cubes
}

proc getIntersection {c1 c2} {
    set s1 [lindex $c1 0]
    set x1_1 [lindex $c1 1]
    set x2_1 [lindex $c1 2]
    set y1_1 [lindex $c1 3]
    set y2_1 [lindex $c1 4]
    set z1_1 [lindex $c1 5]
    set z2_1 [lindex $c1 6]

    set x1_2 [lindex $c2 1]
    set x2_2 [lindex $c2 2]
    set y1_2 [lindex $c2 3]
    set y2_2 [lindex $c2 4]
    set z1_2 [lindex $c2 5]
    set z2_2 [lindex $c2 6]

    set ix1 [expr {max($x1_1, $x1_2)}]
    set ix2 [expr {min($x2_1, $x2_2)}]
    set iy1 [expr {max($y1_1, $y1_2)}]
    set iy2 [expr {min($y2_1, $y2_2)}]
    set iz1 [expr {max($z1_1, $z1_2)}]
    set iz2 [expr {min($z2_1, $z2_2)}]

    if {$ix1 > $ix2 || $iy1 > $iy2 || $iz1 > $iz2} {
        return [list {} 0]
    }

    set is [expr {!$s1}]

    return [list [list $is $ix1 $ix2 $iy1 $iy2 $iz1 $iz2] 1]
}

proc volume {cube} {
    set s [lindex $cube 0]
    set x1 [lindex $cube 1]
    set x2 [lindex $cube 2]
    set y1 [lindex $cube 3]
    set y2 [lindex $cube 4]
    set z1 [lindex $cube 5]
    set z2 [lindex $cube 6]

    set vol [expr {($x2 - $x1 + 1) * ($y2 - $y1 + 1) * ($z2 - $z1 + 1)}]

    if {$s == 0} {
        return [expr {-$vol}]
    } else {
        return $vol
    }
}

proc solve {cubes} {
    set finalList {}

    foreach c $cubes {
        set toAdd {}
        foreach finalCube $finalList {
            set result [getIntersection $finalCube $c]
            set intersection [lindex $result 0]
            set didIntersect [lindex $result 1]
            if {$didIntersect} {
                lappend toAdd $intersection
            }
        }

        if {[lindex $c 0] == 1} {
            lappend toAdd $c
        }

        set finalList [concat $finalList $toAdd]
    }

    set totalVolume 0
    foreach cube $finalList {
        set totalVolume [expr {$totalVolume + [volume $cube]}]
    }

    return $totalVolume
}

proc main {} {
    set file_id [open "input.txt" r]
    set input_data [read $file_id]
    close $file_id

    set cubes [parseInput $input_data]
    set result [solve $cubes]
    puts $result
}

main
