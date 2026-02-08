
set f [open input.txt r]
set codeStr [split [string trim [read $f]] ,]
close $f

set memory {}
foreach num $codeStr {lappend memory [expr {$num}]}
set ip 0
set halted 0
set inputQ {}
set outputQ {}

proc readMem addr {
    global memory
    if {$addr >= [llength $memory]} {
        lappend memory {*}[lrepeat [expr {$addr - [llength $memory] + 1}] 0]
    }
    lindex $memory $addr
}

proc writeMem {addr val} {
    global memory
    if {$addr >= [llength $memory]} {
        lappend memory {*}[lrepeat [expr {$addr - [llength $memory] + 1}] 0]
    }
    lset memory $addr $val
}

proc getParams count {
    global memory ip
    set modes [expr {[lindex $memory $ip] / 100}]
    set res {}
    for {set i 0} {$i < $count} {incr i} {
        set mode [expr {$modes % 10}]
        set modes [expr {$modes / 10}]
        if {$mode == 1} {
            lappend res [expr {$ip + $i + 1}]
        } else {
            lappend res [readMem [expr {$ip + $i + 1}]]
        }
    }
    return $res
}

proc run {} {
    global ip halted inputQ outputQ
    set outputQ {}
    while {!$halted} {
        set op [expr {[readMem $ip] % 100}]
        switch $op {
            1 - 2 - 7 - 8 {
                set p [getParams 3]
                set a [readMem [lindex $p 0]]
                set b [readMem [lindex $p 1]]
                if {$op == 1} {set c [expr {$a + $b}]} \
                elseif {$op == 2} {set c [expr {$a * $b}]} \
                elseif {($op == 7 && $a < $b) || ($op == 8 && $a == $b)} {set c 1} \
                else {set c 0}
                writeMem [lindex $p 2] $c
                incr ip 4
            }
            3 - 4 {
                set p [getParams 1]
                if {$op == 3} {
                    if {[llength $inputQ] == 0} return
                    set val [lindex $inputQ 0]
                    set inputQ [lrange $inputQ 1 end]
                    writeMem [lindex $p 0] $val
                } else {
                    lappend outputQ [readMem [lindex $p 0]]
                }
                incr ip 2
            }
            5 - 6 {
                set p [getParams 2]
                set a [readMem [lindex $p 0]]
                set b [readMem [lindex $p 1]]
                if {($op == 5 && $a != 0) || ($op == 6 && $a == 0)} {
                    set ip $b
                } else {
                    incr ip 3
                }
            }
            99 {set halted 1}
            default {error "bad op $op"}
        }
    }
}

proc addInput x {global inputQ; lappend inputQ $x}

array set grid {}
set x 0
set y 0
set dir 0 ;# 0 up 1 right 2 down 3 left

while {!$halted} {
    set key "$x,$y"
    if {![info exists grid($key)]} {set grid($key) 0}
    addInput $grid($key)
    run
    if {[llength $outputQ] == 2} {
        set grid($key) [lindex $outputQ 0]
        set turn [lindex $outputQ 1]
        if {$turn == 0} {set dir [expr {($dir + 3) % 4}]} \
        else {set dir [expr {($dir + 1) % 4}]}
        switch $dir {
            0 {incr y -1}
            1 {incr x}
            2 {incr y}
            3 {incr x -1}
        }
    }
}

puts [array size grid]
