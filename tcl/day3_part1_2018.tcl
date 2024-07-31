proc ParseClaim {line} {
    set regex {#(\d+) @ (\d+),(\d+): (\d+)x(\d+)}
    if {[regexp $regex $line match id left top width height]} {
        return [list $id $left $top $width $height]
    }
    return {}
}

proc ReadClaims {filename} {
    set claims {}
    set file [open $filename r]
    while {[gets $file line] >= 0} {
        lappend claims [ParseClaim $line]
    }
    close $file
    return $claims
}

proc CountOverlappingInches {claims} {
    array set fabric {}
    foreach claim $claims {
        set left [lindex $claim 1]
        set top [lindex $claim 2]
        set width [lindex $claim 3]
        set height [lindex $claim 4]
        for {set i $left} {$i < $left + $width} {incr i} {
            for {set j $top} {$j < $top + $height} {incr j} {
                set coord "$i,$j"
                incr fabric($coord) 1
            }
        }
    }
    set overlapping 0
    foreach count [array names fabric] {
        if {[set fabric($count)] > 1} {
            incr overlapping
        }
    }
    return $overlapping
}

set claims [ReadClaims "input.txt"]
set overlapping [CountOverlappingInches $claims]
puts $overlapping