
#!/usr/bin/env tclsh

# Read input file
proc readInput {filename} {
    set fp [open $filename r]
    set data [read $fp]
    close $fp
    return [split [string trim $data] ","]
}

# Intcode VM class-like implementation
oo::class create IntcodeVM {
    variable code ip relativeBase input output

    constructor {filename} {
        set code [dict create]
        set ip 0
        set relativeBase 0
        set input {}
        set output {}
        my loadProgram $filename
    }

    method loadProgram {filename} {
        set rawCode [readInput $filename]
        for {set i 0} {$i < [llength $rawCode]} {incr i} {
            dict set code $i [lindex $rawCode $i]
        }
    }

    method getParamAddress {pos mode} {
        switch $mode {
            0 { return [dict get $code $pos] }
            1 { return $pos }
            2 { return [expr {$relativeBase + [dict get $code $pos]}] }
        }
    }

    method getParamsAddresses {pos cmd arity} {
        set results {}
        for {set i 0} {$i < $arity} {incr i} {
            set mode [my extractMode $cmd $i]
            lappend results [my getParamAddress [expr {$pos + $i + 1}] $mode]
        }
        return $results
    }

    method extractMode {cmd index} {
        return [expr {($cmd / int(pow(10, $index + 2))) % 10}]
    }

    method run {} {
        while {1} {
            set cmd [dict get $code $ip]
            set opcode [expr {$cmd % 100}]

            switch $opcode {
                1 {
                    set params [my getParamsAddresses $ip $cmd 3]
                    dict set code [lindex $params 2] [expr {[dict get $code [lindex $params 0]] + [dict get $code [lindex $params 1]]}]
                    incr ip 4
                }
                2 {
                    set params [my getParamsAddresses $ip $cmd 3]
                    dict set code [lindex $params 2] [expr {[dict get $code [lindex $params 0]] * [dict get $code [lindex $params 1]]}]
                    incr ip 4
                }
                3 {
                    set params [my getParamsAddresses $ip $cmd 1]
                    dict set code [lindex $params 0] [lindex $input 0]
                    set input [lrange $input 1 end]
                    incr ip 2
                }
                4 {
                    set params [my getParamsAddresses $ip $cmd 1]
                    lappend output [dict get $code [lindex $params 0]]
                    incr ip 2
                }
                5 {
                    set params [my getParamsAddresses $ip $cmd 2]
                    if {[dict get $code [lindex $params 0]] != 0} {
                        set ip [dict get $code [lindex $params 1]]
                    } else {
                        incr ip 3
                    }
                }
                6 {
                    set params [my getParamsAddresses $ip $cmd 2]
                    if {[dict get $code [lindex $params 0]] == 0} {
                        set ip [dict get $code [lindex $params 1]]
                    } else {
                        incr ip 3
                    }
                }
                7 {
                    set params [my getParamsAddresses $ip $cmd 3]
                    dict set code [lindex $params 2] [expr {[dict get $code [lindex $params 0]] < [dict get $code [lindex $params 1]] ? 1 : 0}]
                    incr ip 4
                }
                8 {
                    set params [my getParamsAddresses $ip $cmd 3]
                    dict set code [lindex $params 2] [expr {[dict get $code [lindex $params 0]] == [dict get $code [lindex $params 1]] ? 1 : 0}]
                    incr ip 4
                }
                9 {
                    set params [my getParamsAddresses $ip $cmd 1]
                    incr relativeBase [dict get $code [lindex $params 0]]
                    incr ip 2
                }
                99 {
                    break
                }
                default {
                    error "Invalid opcode: $opcode"
                }
            }
        }
        return $output
    }

    method setInput {inputList} {
        set input $inputList
    }

    method getOutput {} {
        return $output
    }
}

# Main execution
proc main {} {
    set vm [IntcodeVM new "input.txt"]
    
    set instructions {
        "NOT A J"
        "NOT B T"
        "OR T J"
        "NOT C T"
        "OR T J"
        "AND D J"
        "WALK"
    }

    set inputChars {}
    foreach instr $instructions {
        foreach char [split $instr ""] {
            lappend inputChars [scan $char %c]
        }
        lappend inputChars 10
    }

    $vm setInput $inputChars
    set output [$vm run]

    foreach val $output {
        if {$val > 127} {
            puts $val
        } else {
            puts -nonewline [format %c $val]
        }
    }
}

main
