set file [open "input.txt" r]
set genAStart [gets $file]
set genBStart [gets $file]
close $file

set genAFactor 16807
set genBFactor 48271
set modulus 2147483647

set genA $genAStart
set genB $genBStart
set matches 0

for {set i 0} {$i < 5000000} {incr i} {
    while {1} {
        set genA [expr {($genA * $genAFactor) % $modulus}]
        if {($genA % 4) == 0} {break}
    }

    while {1} {
        set genB [expr {($genB * $genBFactor) % $modulus}]
        if {($genB % 8) == 0} {break}
    }

    if {($genA & 0xFFFF) == ($genB & 0xFFFF)} {
        incr matches
    }
}

puts $matches