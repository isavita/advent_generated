
# Read program
set f [open "input.txt"]
set data [string trim [read $f]]
close $f
set prog [split $data ","]
array set mem {}
for {set i 0} {$i < [llength $prog]} {incr i} {
    set mem($i) [expr {[lindex $prog $i] + 0}]
}
set ptr 0
set relBase 0
set inputs {}
set outputs {}
set halted 0

# Parameter handling
proc getParam {mode param} {
    global mem relBase
    if {$mode == 0} {
        if {![info exists mem($param)]} {set mem($param) 0}
        return $mem($param)
    } elseif {$mode == 1} {
        return $param
    } else {
        set addr [expr {$relBase + $param}]
        if {![info exists mem($addr)]} {set mem($addr) 0}
        return $mem($addr)
    }
}
proc setParam {mode param value} {
    global mem relBase
    if {$mode == 0} {
        set mem($param) $value
    } else {
        set addr [expr {$relBase + $param}]
        set mem($addr) $value
    }
}

# Intcode execution
proc runComputer {} {
    global ptr relBase mem inputs outputs halted
    while {1} {
        if {![info exists mem($ptr)]} {set mem($ptr) 0}
        set instr $mem($ptr)
        set opcode [expr {$instr % 100}]
        set mode1 [expr {int($instr/100) % 10}]
        set mode2 [expr {int($instr/1000) % 10}]
        set mode3 [expr {int($instr/10000) % 10}]
        if {$opcode == 99} {set halted 1; return}
        set p1 [expr {$ptr+1}]
        set p2 [expr {$ptr+2}]
        set p3 [expr {$ptr+3}]
        foreach idx {$p1 $p2 $p3} {
            if {![info exists mem($idx)]} {set mem($idx) 0}
        }
        if {$opcode == 1} {
            set v1 [getParam $mode1 $mem($p1)]
            set v2 [getParam $mode2 $mem($p2)]
            setParam $mode3 $mem($p3) [expr {$v1+$v2}]
            set ptr [expr {$ptr+4}]
        } elseif {$opcode == 2} {
            set v1 [getParam $mode1 $mem($p1)]
            set v2 [getParam $mode2 $mem($p2)]
            setParam $mode3 $mem($p3) [expr {$v1*$v2}]
            set ptr [expr {$ptr+4}]
        } elseif {$opcode == 3} {
            if {[llength $inputs] == 0} {return}
            set in [lindex $inputs 0]
            set inputs [lrange $inputs 1 end]
            setParam $mode1 $mem($p1) $in
            set ptr [expr {$ptr+2}]
        } elseif {$opcode == 4} {
            set v [getParam $mode1 $mem($p1)]
            lappend outputs $v
            set ptr [expr {$ptr+2}]
            return
        } elseif {$opcode == 5} {
            set v1 [getParam $mode1 $mem($p1)]
            set v2 [getParam $mode2 $mem($p2)]
            if {$v1 != 0} {set ptr $v2} else {set ptr [expr {$ptr+3}]}
        } elseif {$opcode == 6} {
            set v1 [getParam $mode1 $mem($p1)]
            set v2 [getParam $mode2 $mem($p2)]
            if {$v1 == 0} {set ptr $v2} else {set ptr [expr {$ptr+3}]}
        } elseif {$opcode == 7} {
            set v1 [getParam $mode1 $mem($p1)]
            set v2 [getParam $mode2 $mem($p2)]
            setParam $mode3 $mem($p3) [expr {$v1 < $v2 ? 1 : 0}]
            set ptr [expr {$ptr+4}]
        } elseif {$opcode == 8} {
            set v1 [getParam $mode1 $mem($p1)]
            set v2 [getParam $mode2 $mem($p2)]
            setParam $mode3 $mem($p3) [expr {$v1 == $v2 ? 1 : 0}]
            set ptr [expr {$ptr+4}]
        } elseif {$opcode == 9} {
            set v1 [getParam $mode1 $mem($p1)]
            set relBase [expr {$relBase + $v1}]
            set ptr [expr {$ptr+2}]
        } else {
            error "unknown opcode $opcode"
        }
    }
}
proc getOutput {} {
    global outputs
    set out [lindex $outputs 0]
    set outputs [lrange $outputs 1 end]
    return $out
}

# Directions
set dx(1) 0; set dy(1) -1
set dx(2) 0; set dy(2) 1
set dx(3) -1; set dy(3) 0
set dx(4) 1; set dy(4) 0
set opp(1) 2; set opp(2) 1; set opp(3) 4; set opp(4) 3

# Map exploration
array set visited {}
array set map {}
set visited(0,0) 1
set map(0,0) 1
set oxyX {}
set oxyY {}
proc dfs {x y} {
    global visited map inputs outputs dx dy opp oxyX oxyY
    foreach cmd {1 2 3 4} {
        set nx [expr {$x + $dx($cmd)}]
        set ny [expr {$y + $dy($cmd)}]
        set key "$nx,$ny"
        if {[info exists visited($key)]} continue
        lappend inputs $cmd
        runComputer
        set status [getOutput]
        if {$status == 0} {
            set map($key) 0
        } else {
            set map($key) $status
            set visited($key) 1
            if {$status == 2} {set oxyX $nx; set oxyY $ny}
            dfs $nx $ny
            set back $opp($cmd)
            lappend inputs $back
            runComputer
            getOutput
        }
    }
}
dfs 0 0

# BFS shortest path
proc bfs {start end} {
    global map dx dy
    set queue [list $start 0]
    array set seen {}
    set seen($start) 1
    while {[llength $queue] > 0} {
        set key [lindex $queue 0]
        set steps [lindex $queue 1]
        set queue [lrange $queue 2 end]
        if {$key eq $end} {return $steps}
        foreach cmd {1 2 3 4} {
            foreach {x y} [split $key ,] {}
            set nx [expr {$x + $dx($cmd)}]
            set ny [expr {$y + $dy($cmd)}]
            set nkey "$nx,$ny"
            if {[info exists map($nkey)] && $map($nkey) != 0 && ![info exists seen($nkey)]} {
                set seen($nkey) 1
                lappend queue $nkey [expr {$steps+1}]
            }
        }
    }
    return -1
}

# Oxygen spread simulation
proc spread {start} {
    global map dx dy
    set queue [list $start 0]
    array set seen {}
    set seen($start) 1
    set max 0
    while {[llength $queue] > 0} {
        set key [lindex $queue 0]
        set mins [lindex $queue 1]
        set queue [lrange $queue 2 end]
        if {$mins > $max} {set max $mins}
        foreach cmd {1 2 3 4} {
            foreach {x y} [split $key ,] {}
            set nx [expr {$x + $dx($cmd)}]
            set ny [expr {$y + $dy($cmd)}]
            set nkey "$nx,$ny"
            if {[info exists map($nkey)] && $map($nkey) != 0 && ![info exists seen($nkey)]} {
                set seen($nkey) 1
                lappend queue $nkey [expr {$mins+1}]
            }
        }
    }
    return $max
}

set startKey "0,0"
set endKey "$oxyX,$oxyY"
puts [bfs $startKey $endKey]
puts [spread $endKey]
