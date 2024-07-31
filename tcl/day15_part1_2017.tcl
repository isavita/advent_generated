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

for {set i 0} {$i < 40000000} {incr i} {
    set genA [expr {($genA * $genAFactor) % $modulus}]
    set genB [expr {($genB * $genBFactor) % $modulus}]
    if {($genA & 0xFFFF) == ($genB & 0xFFFF)} {
        incr matches
    }
}

puts $matches