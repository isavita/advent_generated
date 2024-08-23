proc reactPolymer {polymer} {
    set stack {}
    foreach unit [split $polymer ""] {
        if {[llength $stack] > 0} {
            set top [lindex $stack end]
            if {[string tolower $unit] eq [string tolower $top] && $unit ne $top} {
                set stack [lrange $stack 0 end-1]
            } else {
                lappend stack $unit
            }
        } else {
            lappend stack $unit
        }
    }
    return [llength $stack]
}

proc removeUnitType {polymer unitType} {
    set result ""
    foreach unit [split $polymer ""] {
        if {[string tolower $unit] ne $unitType} {
            append result $unit
        }
    }
    return $result
}

set file [open "input.txt" r]
set polymer [read $file]
close $file

set shortest [string length $polymer]

foreach unitType {a b c d e f g h i j k l m n o p q r s t u v w x y z} {
    set modifiedPolymer [removeUnitType $polymer $unitType]
    set length [reactPolymer $modifiedPolymer]
    if {$length < $shortest} {
        set shortest $length
    }
}

puts $shortest