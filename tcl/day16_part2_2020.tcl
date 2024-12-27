
proc parse_input {filename} {
    set rules {}
    set my_ticket {}
    set nearby_tickets {}
    set section "rules"

    set file [open $filename r]
    while {[gets $file line] >= 0} {
        if {$line eq ""} {
            continue
        } elseif {$line eq "your ticket:"} {
            set section "my_ticket"
        } elseif {$line eq "nearby tickets:"} {
            set section "nearby_tickets"
        } else {
            switch $section {
                rules {
                    regexp {([^:]+): (\d+)-(\d+) or (\d+)-(\d+)} $line _ field r1_start r1_end r2_start r2_end
                    lappend rules [list $field [list $r1_start $r1_end] [list $r2_start $r2_end]]
                }
                my_ticket {
                    set my_ticket [split $line ,]
                }
                nearby_tickets {
                    lappend nearby_tickets [split $line ,]
                }
            }
        }
    }
    close $file
    return [list $rules $my_ticket $nearby_tickets]
}

proc is_valid_value {value rules} {
    foreach rule $rules {
        set ranges [lrange $rule 1 end]
        foreach range $ranges {
            if {$value >= [lindex $range 0] && $value <= [lindex $range 1]} {
                return 1
            }
        }
    }
    return 0
}

proc calculate_error_rate {nearby_tickets rules} {
    set error_rate 0
    set valid_tickets {}
    foreach ticket $nearby_tickets {
        set is_valid_ticket 1
        foreach value $ticket {
            if {![is_valid_value $value $rules]} {
                incr error_rate $value
                set is_valid_ticket 0
            }
        }
        if {$is_valid_ticket} {
            lappend valid_tickets $ticket
        }
    }
    return [list $error_rate $valid_tickets]
}

proc determine_field_order {valid_tickets rules} {
    set num_fields [llength [lindex $valid_tickets 0]]
    set possible_fields {}
    for {set i 0} {$i < $num_fields} {incr i} {
        set possible_fields_for_pos {}
        foreach rule $rules {
            set field [lindex $rule 0]
            set is_possible 1
            foreach ticket $valid_tickets {
                set value [lindex $ticket $i]
                if {![is_valid_value $value [list $rule]]} {
                    set is_possible 0
                    break
                }
            }
            if {$is_possible} {
                lappend possible_fields_for_pos $field
            }
        }
        lappend possible_fields $possible_fields_for_pos
    }

    set field_order {}
    set assigned_fields {}
    while {[llength $field_order] < $num_fields} {
        for {set i 0} {$i < $num_fields} {incr i} {
            set possible_fields_for_pos [lindex $possible_fields $i]
            set unassigned_fields {}
            foreach field $possible_fields_for_pos {
                if {[lsearch $assigned_fields $field] == -1} {
                    lappend unassigned_fields $field
                }
            }
            if {[llength $unassigned_fields] == 1} {
                lappend field_order [list $i [lindex $unassigned_fields 0]]
                lappend assigned_fields [lindex $unassigned_fields 0]
            }
        }
    }
    return $field_order
}

proc calculate_departure_product {my_ticket field_order} {
    set product 1
    foreach field_info $field_order {
        set index [lindex $field_info 0]
        set field [lindex $field_info 1]
        if {[string match "departure*" $field]} {
            set value [lindex $my_ticket $index]
            set product [expr {$product * $value}]
        }
    }
    return $product
}

# Main program
set filename "input.txt"
set data [parse_input $filename]
set rules [lindex $data 0]
set my_ticket [lindex $data 1]
set nearby_tickets [lindex $data 2]

set error_data [calculate_error_rate $nearby_tickets $rules]
set error_rate [lindex $error_data 0]
set valid_tickets [lindex $error_data 1]

puts "Part 1 Error Rate: $error_rate"

set field_order [determine_field_order $valid_tickets $rules]
set departure_product [calculate_departure_product $my_ticket $field_order]
puts "Part 2 Departure Product: $departure_product"
