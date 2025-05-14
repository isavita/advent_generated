
proc readMonkeys {filename} {
    set monkeys [list]
    if {[catch {open $filename r} file_id result]} {
        puts stderr "Error opening file $filename: $result"
        return ""
    }

    set monkey_data [dict create]
    while {[gets $file_id line] >= 0} {
        set line [string trim $line]
        if {[string match "Monkey*" $line]} {
            set monkey_data [dict create items [list] op "" operand "" test "" true "" false "" count 0]
        } elseif {[string match "Starting items:*" $line]} {
            set items_str [string trim [string range $line [string first ":" $line]+1 end]]
            set items [list]
            foreach item_str [split $items_str ","] {
                lappend items [string trim $item_str]
            }
            dict set monkey_data items $items
        } elseif {[string match "Operation:*" $line]} {
            set parts [split $line " "]
            set op [lindex $parts 4]
            set operand [lindex $parts 5]
            dict set monkey_data op $op
            dict set monkey_data operand $operand
        } elseif {[string match "Test:*" $line]} {
            set parts [split $line " "]
            set test_divisor [lindex $parts 3]
            dict set monkey_data test $test_divisor
        } elseif {[string match "If true:*" $line]} {
            set parts [split $line " "]
            set true_monkey [lindex $parts 5]
            dict set monkey_data true $true_monkey
        } elseif {[string match "If false:*" $line]} {
            set parts [split $line " "]
            set false_monkey [lindex $parts 5]
            dict set monkey_data false $false_monkey
            lappend monkeys $monkey_data
        }
    }
    close $file_id
    return $monkeys
}

proc main {} {
    set filename "input.txt"
    set monkeys [readMonkeys $filename]
    if {$monkeys eq ""} {
        exit 1
    }
    set rounds 20

    for {set r 0} {$r < $rounds} {incr r} {
        for {set i 0} {$i < [llength $monkeys]} {incr i} {
            set monkey_idx $i
            set monkey [lindex $monkeys $monkey_idx]

            set items [dict get $monkey items]
            set op [dict get $monkey op]
            set operand [dict get $monkey operand]
            set test_divisor [dict get $monkey test]
            set true_monkey [dict get $monkey true]
            set false_monkey [dict get $monkey false]
            set inspect_count [dict get $monkey count]

            set new_items_to_throw [list]

            foreach item $items {
                incr inspect_count
                
                set current_operand $operand
                if {$current_operand eq "old"} {
                    set current_operand $item
                }

                if {$op eq "+"} {
                    set new_value [expr {$item + $current_operand}]
                } elseif {$op eq "*"} {
                    set new_value [expr {$item * $current_operand}]
                } else {
                    set new_value $item
                }

                set new_value [expr {int($new_value / 3)}]

                if {$new_value % $test_divisor == 0} {
                    lappend new_items_to_throw [list $true_monkey $new_value]
                } else {
                    lappend new_items_to_throw [list $false_monkey $new_value]
                }
            }
            
            dict set monkey count $inspect_count
            dict set monkey items [list]
            lset monkeys $monkey_idx $monkey

            foreach item_info $new_items_to_throw {
                set target_monkey_idx [lindex $item_info 0]
                set item_to_add [lindex $item_info 1]
                
                set target_monkey [lindex $monkeys $target_monkey_idx]
                set current_target_items [dict get $target_monkey items]
                lappend current_target_items $item_to_add
                dict set target_monkey items $current_target_items
                lset monkeys $target_monkey_idx $target_monkey
            }
        }
    }

    set inspect_counts [list]
    foreach monkey $monkeys {
        lappend inspect_counts [dict get $monkey count]
    }

    set sorted_counts [lsort -integer -decreasing $inspect_counts]

    set top1 [lindex $sorted_counts 0]
    set top2 [lindex $sorted_counts 1]

    set monkey_business [expr {$top1 * $top2}]

    puts $monkey_business
}

main
