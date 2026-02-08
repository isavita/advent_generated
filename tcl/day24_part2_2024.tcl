#!/usr/bin/tclsh

set fp [open "input.txt" r]
set data [read $fp]
close $fp

set defs [string trimright [string trimleft $data]]
if {[string first "\n\n" $defs] >= 0} {
    set defs [string range $defs [expr {[string first "\n\n" $defs] + 2}] end]
}

array set gate_a {}
array set gate_b {}
array set gate_op {}
array set gate_output {}
set gate_count 0
set numZ 0

foreach line [split $defs "\n"] {
    if {[string trim $line] eq ""} continue
    regexp {(\S+)\s+(\S+)\s+(\S+)\s+->\s+(\S+)} $line -> a op b out
    set gate_a($gate_count) $a
    set gate_b($gate_count) $b
    set gate_op($gate_count) $op
    set gate_output($gate_count) $out
    if {[string index $out 0] eq "z"} { incr numZ }
    incr gate_count
}

proc find_output_by_gate {a1 op b1} {
    upvar gate_count gate_count gate_a gate_a gate_b gate_b gate_op gate_op gate_output gate_output
    for {set i 0} {$i < $gate_count} {incr i} {
        if {$gate_op($i) eq $op} {
            if {[string equal $gate_a($i) $a1] && [string equal $gate_b($i) $b1]} {
                return $gate_output($i)
            }
            if {[string equal $gate_a($i) $b1] && [string equal $gate_b($i) $a1]} {
                return $gate_output($i)
            }
        }
    }
    return ""
}

proc find_gate_by_output {out} {
    upvar gate_count gate_count gate_a gate_a gate_b gate_b gate_op gate_op gate_output gate_output
    for {set i 0} {$i < $gate_count} {incr i} {
        if {[string equal $gate_output($i) $out]} { return $i }
    }
    return -1
}

proc swap_gate_outputs {out1 out2} {
    upvar gate_count gate_count gate_output gate_output
    for {set i 0} {$i < $gate_count} {incr i} {
        if {[string equal $gate_output($i) $out1]} {
            set gate_output($i) $out2
        } elseif {[string equal $gate_output($i) $out2]} {
            set gate_output($i) $out1
        }
    }
}

set swapped_pairs {}
set pair_count 0

while {$pair_count < 4} {
    set carry ""
    for {set i 0} {$i < $numZ && $pair_count < 4} {incr i} {
        set xi [format "x%02d" $i]
        set yi [format "y%02d" $i]
        set zi [format "z%02d" $i]

        set adder ""
        set next_carry ""

        if {$i == 0} {
            set adder [find_output_by_gate $xi XOR $yi]
            set next_carry [find_output_by_gate $xi AND $yi]
        } else {
            set bit [find_output_by_gate $xi XOR $yi]
            if {[string length $bit] && [string length $carry]} {
                set adder [find_output_by_gate $bit XOR $carry]
                set c1 [find_output_by_gate $xi AND $yi]
                set c2 [find_output_by_gate $bit AND $carry]
                if {[string length $c1] && [string length $c2]} {
                    set next_carry [find_output_by_gate $c1 OR $c2]
                }
            }
        }

        set swapped 0
        if {[string length $adder] && ![string equal $adder $zi]} {
            lappend swapped_pairs $adder
            lappend swapped_pairs $zi
            swap_gate_outputs $adder $zi
            set swapped 1
        } elseif {[string length $adder] == 0} {
            set zidx [find_gate_by_output $zi]
            if {$zidx >= 0} {
                set bit [find_output_by_gate $xi XOR $yi]
                if {[string length $bit] && [string length $carry]} {
                    set a1 $gate_a($zidx)
                    set b1 $gate_b($zidx)
                    if {[find_output_by_gate $a1 XOR $carry] != ""} {
                        lappend swapped_pairs $bit
                        lappend swapped_pairs $a1
                        swap_gate_outputs $bit $a1
                        set swapped 1
                    } elseif {[find_output_by_gate $b1 XOR $carry] != ""} {
                        lappend swapped_pairs $bit
                        lappend swapped_pairs $b1
                        swap_gate_outputs $bit $b1
                        set swapped 1
                    }
                }
            }
        }

        if {$swapped} {
            incr pair_count
            break
        }
        set carry $next_carry
    }
}

set sorted [lsort $swapped_pairs]
puts [join $sorted ,]