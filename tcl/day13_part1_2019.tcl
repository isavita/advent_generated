set filename "input.txt"
set fp [open $filename r]
set data [read $fp]
close $fp
set program [split $data ,]

namespace eval Mode {
    variable POSITION 0
    variable IMMEDIATE 1
    variable RELATIVE 2
}

namespace eval Opcode {
    variable ADD 1
    variable MUL 2
    variable INPUT 3
    variable OUTPUT 4
    variable JT 5
    variable JF 6
    variable LT 7
    variable EQ 8
    variable RBO 9
    variable HALT 99
}

proc decode {n} {
    set op [expr {$n % 100}]
    set n [expr {$n / 100}]
    set modes [list [expr {$n % 10}] [expr {($n / 10) % 10}] [expr {$n / 100}]]
    return [list $op $modes]
}

proc run {program} {
    array set data {}
    set idx 0
    foreach val $program {
        set data($idx) $val
        incr idx
    }
    
    set out_stream {}
    set ip 0
    set rel_base 0
    
    while {1} {
        lassign [decode $data($ip)] op modes
        if {$op == $::Opcode::ADD} {
            set a [get_val data $ip 0 [lindex $modes 0] $rel_base]
            set b [get_val data $ip 1 [lindex $modes 1] $rel_base]
            set_val data $ip 2 [lindex $modes 2] [expr {$a + $b}] $rel_base
            incr ip 4
        } elseif {$op == $::Opcode::MUL} {
            set a [get_val data $ip 0 [lindex $modes 0] $rel_base]
            set b [get_val data $ip 1 [lindex $modes 1] $rel_base]
            set_val data $ip 2 [lindex $modes 2] [expr {$a * $b}] $rel_base
            incr ip 4
        } elseif {$op == $::Opcode::INPUT} {
            set_val data $ip 0 [lindex $modes 0] 0 $rel_base
            incr ip 2
        } elseif {$op == $::Opcode::OUTPUT} {
            set val [get_val data $ip 0 [lindex $modes 0] $rel_base]
            lappend out_stream $val
            incr ip 2
        } elseif {$op == $::Opcode::JT} {
            set a [get_val data $ip 0 [lindex $modes 0] $rel_base]
            set b [get_val data $ip 1 [lindex $modes 1] $rel_base]
            if {$a != 0} {
                set ip $b
            } else {
                incr ip 3
            }
        } elseif {$op == $::Opcode::JF} {
            set a [get_val data $ip 0 [lindex $modes 0] $rel_base]
            set b [get_val data $ip 1 [lindex $modes 1] $rel_base]
            if {$a == 0} {
                set ip $b
            } else {
                incr ip 3
            }
        } elseif {$op == $::Opcode::LT} {
            set a [get_val data $ip 0 [lindex $modes 0] $rel_base]
            set b [get_val data $ip 1 [lindex $modes 1] $rel_base]
            set_val data $ip 2 [lindex $modes 2] [expr {$a < $b ? 1 : 0}] $rel_base
            incr ip 4
        } elseif {$op == $::Opcode::EQ} {
            set a [get_val data $ip 0 [lindex $modes 0] $rel_base]
            set b [get_val data $ip 1 [lindex $modes 1] $rel_base]
            set_val data $ip 2 [lindex $modes 2] [expr {$a == $b ? 1 : 0}] $rel_base
            incr ip 4
        } elseif {$op == $::Opcode::RBO} {
            set a [get_val data $ip 0 [lindex $modes 0] $rel_base]
            incr rel_base $a
            incr ip 2
        } elseif {$op == $::Opcode::HALT} {
            break
        } else {
            error "Unknown opcode: $op"
        }
    }
    
    return $out_stream
}

proc get_val {data_var ip offset mode rel_base} {
    upvar $data_var data
    set addr [expr {$ip + $offset + 1}]
    if {$mode == $::Mode::IMMEDIATE} {
        return $data($addr)
    } elseif {$mode == $::Mode::POSITION} {
        return $data($data($addr))
    } elseif {$mode == $::Mode::RELATIVE} {
        return $data([expr {$rel_base + $data($addr)}])
    }
    error "Unknown mode: $mode"
}

proc set_val {data_var ip offset mode val rel_base} {
    upvar $data_var data
    set addr [expr {$ip + $offset + 1}]
    if {$mode == $::Mode::POSITION} {
        set data($data($addr)) $val
    } elseif {$mode == $::Mode::RELATIVE} {
        set data([expr {$rel_base + $data($addr)}]) $val
    } else {
        error "Unknown mode: $mode"
    }
}

proc count_blocks {program} {
    set out_stream [run $program]
    set grid {}
    set count 0
    while {[llength $out_stream] >= 3} {
        set x [lindex $out_stream 0]
        set y [lindex $out_stream 1]
        set t [lindex $out_stream 2]
        set out_stream [lrange $out_stream 3 end]
        if {$t == 2} {
            incr count
        }
    }
    return $count
}

puts [count_blocks $program]