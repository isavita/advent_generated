
proc main {} {
    set program [loadProgram "input.txt"]

    set y 20
    set x 0

    while {true} {
        if {[beam $program $x $y] == 0} {
            incr x
            continue
        }

        if {[beam $program [expr {$x + 99}] $y] == 0} {
            incr y
            continue
        }

        if {[beam $program $x [expr {$y + 99}]] == 0} {
            incr x
            continue
        }

        puts [expr {$x * 10000 + $y}]
        return
    }
}

proc loadProgram {filename} {
    set f [open $filename r]
    set line [read $f]
    close $f

    set code {}
    set parts [split [string trim $line] ","]
    for {set i 0} {$i < [llength $parts]} {incr i} {
        dict set code $i [lindex $parts $i]
    }
    return $code
}

proc runVM {initialMemory inputList} {
    set code [dict create {*}$initialMemory]
    set ip 0
    set relativeBase 0
    set outputList {}
    set inputIdx 0

    proc getMem {mem ip} {
        if {[dict exists $mem $ip]} {
            return [dict get $mem $ip]
        }
        return 0
    }

    proc setMem {mem ip value} {
        dict set mem $ip $value
        return $mem
    }

    proc getParamAddress {code pos mode relativeBase} {
        switch $mode {
            0 {
                set addr [getMem $code $pos]
                return $addr
            }
            1 {
                return $pos
            }
            2 {
                set offset [getMem $code $pos]
                return [expr {$relativeBase + $offset}]
            }
            default {
                error "Invalid mode: $mode"
            }
        }
    }

    proc getModes {instruction arity} {
        set modeSection [expr {$instruction / 100}]
        set modes {}
        for {set i 0} {$i < $arity} {incr i} {
            lappend modes [expr {($modeSection / [expr {int(pow(10, $i))}]) % 10}]
        }
        return $modes
    }

    proc getParamsAddresses {code ip instruction arity relativeBase} {
        set modes [getModes $instruction $arity]
        set addresses {}
        for {set i 0} {$i < $arity} {incr i} {
            lappend addresses [getParamAddress $code [expr {$ip + $i + 1}] [lindex $modes $i] $relativeBase]
        }
        return $addresses
    }

    while {true} {
        set instruction [getMem $code $ip]
        set opcode [expr {$instruction % 100}]

        switch $opcode {
            1 {
                set arity 3
                set params [getParamsAddresses $code $ip $instruction $arity $relativeBase]
                set addr1 [lindex $params 0]
                set addr2 [lindex $params 1]
                set addr3 [lindex $params 2]

                set val1 [getMem $code $addr1]
                set val2 [getMem $code $addr2]

                set code [setMem $code $addr3 [expr {$val1 + $val2}]]
                incr ip [expr {$arity + 1}]
            }
            2 {
                set arity 3
                set params [getParamsAddresses $code $ip $instruction $arity $relativeBase]
                set addr1 [lindex $params 0]
                set addr2 [lindex $params 1]
                set addr3 [lindex $params 2]

                set val1 [getMem $code $addr1]
                set val2 [getMem $code $addr2]

                set code [setMem $code $addr3 [expr {$val1 * $val2}]]
                incr ip [expr {$arity + 1}]
            }
            3 {
                set arity 1
                set params [getParamsAddresses $code $ip $instruction $arity $relativeBase]
                set addr1 [lindex $params 0]

                if {$inputIdx < [llength $inputList]} {
                    set inputValue [lindex $inputList $inputIdx]
                    incr inputIdx
                    set code [setMem $code $addr1 $inputValue]
                    incr ip [expr {$arity + 1}]
                } else {
                     error "VM stalled waiting for input at ip $ip"
                }
            }
            4 {
                set arity 1
                set params [getParamsAddresses $code $ip $instruction $arity $relativeBase]
                set addr1 [lindex $params 0]
                lappend outputList [getMem $code $addr1]
                incr ip [expr {$arity + 1}]
            }
            5 {
                set arity 2
                set params [getParamsAddresses $code $ip $instruction $arity $relativeBase]
                set addr1 [lindex $params 0]
                set addr2 [lindex $params 1]

                set val1 [getMem $code $addr1]
                set val2 [getMem $code $addr2]

                if {$val1 != 0} {
                    set ip $val2
                } else {
                    incr ip [expr {$arity + 1}]
                }
            }
            6 {
                set arity 2
                set params [getParamsAddresses $code $ip $instruction $arity $relativeBase]
                set addr1 [lindex $params 0]
                set addr2 [lindex $params 1]

                set val1 [getMem $code $addr1]
                set val2 [getMem $code $addr2]

                if {$val1 == 0} {
                    set ip $val2
                } else {
                    incr ip [expr {$arity + 1}]
                }
            }
            7 {
                set arity 3
                set params [getParamsAddresses $code $ip $instruction $arity $relativeBase]
                set addr1 [lindex $params 0]
                set addr2 [lindex $params 1]
                set addr3 [lindex $params 2]

                set val1 [getMem $code $addr1]
                set val2 [getMem $code $addr2]

                set code [setMem $code $addr3 [expr {$val1 < $val2 ? 1 : 0}]]
                incr ip [expr {$arity + 1}]
            }
            8 {
                set arity 3
                set params [getParamsAddresses $code $ip $instruction $arity $relativeBase]
                set addr1 [lindex $params 0]
                set addr2 [lindex $params 1]
                set addr3 [lindex $params 2]

                set val1 [getMem $code $addr1]
                set val2 [getMem $code $addr2]

                set code [setMem $code $addr3 [expr {$val1 == $val2 ? 1 : 0}]]
                incr ip [expr {$arity + 1}]
            }
            9 {
                set arity 1
                set params [getParamsAddresses $code $ip $instruction $arity $relativeBase]
                set addr1 [lindex $params 0]
                set val1 [getMem $code $addr1]

                incr relativeBase $val1
                incr ip [expr {$arity + 1}]
            }
            99 {
                break
            }
            default {
                 error "Invalid opcode: $opcode at ip $ip"
            }
        }
    }
    return $outputList
}

proc beam {programTemplate x y} {
    set outputs [runVM $programTemplate [list $x $y]]
    if {[llength $outputs] == 0} {
        return 0
    }
    return [lindex $outputs 0]
}

main
