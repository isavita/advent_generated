
proc reverseSection {arrVar start length n} {
    upvar $arrVar arr
    set i $start
    set j [expr {$start + $length - 1}]
    while {$i < $j} {
        set idx1 [expr {$i % $n}]
        set idx2 [expr {$j % $n}]
        set temp $arr($idx1)
        set arr($idx1) $arr($idx2)
        set arr($idx2) $temp
        incr i
        incr j -1
    }
}

proc knotHash {input} {
    set lengths {}
    foreach c [split $input {}] {
        lappend lengths [scan $c %c]
    }
    set lengths [concat $lengths {17 31 73 47 23}]
    for {set i 0} {$i < 256} {incr i} {set list($i) $i}
    set pos 0
    set skip 0
    for {set round 0} {$round < 64} {incr round} {
        foreach len $lengths {
            reverseSection list $pos $len 256
            set pos [expr {($pos + $len + $skip) % 256}]
            incr skip
        }
    }
    set dense {}
    for {set i 0} {$i < 16} {incr i} {
        set xor 0
        for {set j 0} {$j < 16} {incr j} {
            set idx [expr {$i * 16 + $j}]
            set xor [expr {$xor ^ $list($idx)}]
        }
        lappend dense $xor
    }
    set result ""
    foreach byte $dense {
        append result [format %02x $byte]
    }
    return $result
}

proc hexToBinary {hex} {
    set binary ""
    foreach c [split $hex {}] {
        set n [scan $c %x]
        for {set j 3} {$j >= 0} {incr j -1} {
            append binary [expr {($n & (1 << $j)) ? "1" : "0"}]
        }
    }
    return $binary
}

proc dfs {x y gridVar} {
    upvar $gridVar grid
    if {$x < 0 || $x >= 128 || $y < 0 || $y >= 128 || $grid($x,$y) != 1} return
    set grid($x,$y) 0
    dfs [expr {$x - 1}] $y grid
    dfs [expr {$x + 1}] $y grid
    dfs $x [expr {$y - 1}] grid
    dfs $x [expr {$y + 1}] grid
}

set f [open input.txt r]
set keyString [string trim [gets $f]]
close $f

array set grid {}
set totalUsed 0

for {set i 0} {$i < 128} {incr i} {
    set rowKey "$keyString-$i"
    set hash [knotHash $rowKey]
    set binaryRow [hexToBinary $hash]
    for {set j 0} {$j < 128} {incr j} {
        if {[string index $binaryRow $j] eq "1"} {
            set grid($i,$j) 1
        } else {
            set grid($i,$j) 0
        }
    }
}

set regions 0
for {set i 0} {$i < 128} {incr i} {
    for {set j 0} {$j < 128} {incr j} {
        if {$grid($i,$j) == 1} {
            incr regions
            dfs $i $j grid
        }
    }
}
puts $regions
