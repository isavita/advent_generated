proc reverse {list start length} {
    set n [llength $list]
    for {set i 0} {$i < $length / 2} {incr i} {
        set a [expr {($start + $i) % $n}]
        set b [expr {($start + $length - 1 - $i) % $n}]
        set temp [lindex $list $a]
        set list [lreplace $list $a $a [lindex $list $b]]
        set list [lreplace $list $b $b $temp]
    }
    return $list
}

proc knotHash {input} {
    set lengths {}
    foreach char [split $input ""] {
        lappend lengths [scan $char %c]
    }
    lappend lengths 17 31 73 47 23

    set list [list]
    for {set i 0} {$i < 256} {incr i} {
        lappend list $i
    }

    set currentPosition 0
    set skipSize 0

    for {set round 0} {$round < 64} {incr round} {
        foreach length $lengths {
            set list [reverse $list $currentPosition $length]
            set currentPosition [expr {($currentPosition + $length + $skipSize) % 256}]
            incr skipSize
        }
    }

    set denseHash {}
    for {set i 0} {$i < 256} {incr i 16} {
        set block [lrange $list $i [expr {$i + 15}]]
        set xorResult 0
        foreach num $block {
            set xorResult [expr {$xorResult ^ $num}]
        }
        lappend denseHash $xorResult
    }

    set hexHash ""
    foreach num $denseHash {
        append hexHash [format "%02x" $num]
    }
    return $hexHash
}

set file [open "input.txt" r]
set input [read $file]
close $file

puts [knotHash $input]