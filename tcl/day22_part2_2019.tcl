
proc pow {x y p} {
    set res 1
    set x [expr {$x % $p}]
    while {$y > 0} {
        if {$y & 1} {
            set res [expr {($res * $x) % $p}]
        }
        set y [expr {$y >> 1}]
        set x [expr {($x * $x) % $p}]
    }
    return $res
}

proc modInverse {a m} {
    return [pow $a [expr {$m - 2}] $m]
}

set SIZE 119315717514047
set offset 0
set increment 1

set fileId [open "input.txt" r]
while {[gets $fileId line] >= 0} {
    if {$line eq "deal into new stack"} {
        set increment [expr {-$increment}]
        set offset [expr {($offset + $increment) % $SIZE}]
    } elseif {[string match "cut *" $line]} {
        set n [lindex [split $line " "] end]
        set offset [expr {($offset + $n * $increment) % $SIZE}]
    } elseif {[string match "deal with increment *" $line]} {
        set n [lindex [split $line " "] end]
        set increment [expr {($increment * [modInverse $n $SIZE]) % $SIZE}]
    }
}
close $fileId

set size $SIZE
set iter 101741582076661

set final_increment [pow $increment $iter $size]
set inv_increment_minus_1 [modInverse [expr {($increment - 1 + $size) % $size}] $size]
set final_offset [expr {(([pow $increment $iter $size] - 1 + $size) % $size) * $inv_increment_minus_1 % $size * $offset % $size}]
set answer [expr {(2020 * $final_increment + $final_offset) % $size}]

puts $answer
