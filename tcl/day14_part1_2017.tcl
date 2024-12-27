
proc reverseSection {arr start length} {
    set n [llength $arr]
    for {set i $start; set j [expr {$start + $length - 1}]} {$i < $j} {incr i; incr j -1} {
        set tmp [lindex $arr [expr {$j % $n}]]
        lset arr [expr {$j % $n}] [lindex $arr [expr {$i % $n}]]
        lset arr [expr {$i % $n}] $tmp
    }
    return $arr
}

proc knotHash {input} {
    set lengths {}
    foreach char [split $input ""] {
        lappend lengths [scan $char "%c"]
    }
    lappend lengths 17 31 73 47 23

    set list {}
    for {set i 0} {$i < 256} {incr i} {
        lappend list $i
    }

    set position 0
    set skip 0
    for {set round 0} {$round < 64} {incr round} {
        foreach length $lengths {
            set list [reverseSection $list $position $length]
            set position [expr {($position + $length + $skip) }]
            incr skip
        }
    }

    set denseHash {}
    for {set i 0} {$i < 16} {incr i} {
        set xor 0
        for {set j 0} {$j < 16} {incr j} {
            set xor [expr {$xor ^ [lindex $list [expr {$i * 16 + $j}]]}]
        }
        lappend denseHash $xor
    }

    set hexHash ""
    foreach v $denseHash {
        append hexHash [format %02x $v]
    }
    return $hexHash
}

proc hexToBinary {hexStr} {
    set binaryStr ""
    foreach hexDigit [split $hexStr ""] {
        scan $hexDigit "%x" val
        append binaryStr [format %04b $val]
    }
    return $binaryStr
}

set fp [open "input.txt" r]
set keyString [string trim [read $fp]]
close $fp

set totalUsed 0
for {set i 0} {$i < 128} {incr i} {
    set rowKey [format "%s-%d" $keyString $i]
    set hash [knotHash $rowKey]
    set binaryRow [hexToBinary $hash]
    foreach bit [split $binaryRow ""] {
        if {$bit eq "1"} {
            incr totalUsed
        }
    }
}

puts $totalUsed
