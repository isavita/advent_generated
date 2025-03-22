
# Main entry point
proc main {} {
  set filename "input.txt"
  if {[file exists $filename]} {
    set workflows [dict create]
    set parts [list]
    set reading_workflows 1

    if {[catch {open $filename r} file]} {
      puts "Error opening file: $file"
      return 1
    }

    # Read workflows and parts from file
    while {[gets $file line] >= 0} {
      string trim $line
      if {$line eq ""} {
        set reading_workflows 0
        continue
      }

      if {$reading_workflows} {
        # Parse workflow
        regexp {^([a-z]+)\{(.+)\}$} $line _ name rules_str
        set rules [list]
        foreach rule_str [split $rules_str ","] {
          if {[regexp {^([xmas])([<>])(\d+):([a-zAR]+)$} $rule_str _ category operator value destination]} {
            lappend rules [list $category $operator $value $destination]
          } else {
            lappend rules [list $rule_str]
          }
        }
        dict set workflows $name $rules
      } else {
        # Parse part
        regexp {^\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}$} $line _ x m a s
        lappend parts [dict create x $x m $m a $a s $s]
      }
    }
    close $file

    # Part 1: Process parts through workflows and calculate the sum of accepted parts
    set accepted_sum 0
    foreach part $parts {
      if {[process_part $part $workflows "in"]} {
        set accepted_sum [expr {$accepted_sum + [dict get $part x] + [dict get $part m] + [dict get $part a] + [dict get $part s]}]
      }
    }
    puts "Part 1: $accepted_sum"

    # Part 2: Calculate the number of accepted combinations of ratings
    set initial_ranges [dict create x {1 4000} m {1 4000} a {1 4000} s {1 4000}]
    set accepted_combinations [calculate_accepted_combinations $initial_ranges $workflows "in"]
    puts "Part 2: $accepted_combinations"
  } else {
    puts "Error: File '$filename' not found."
    return 1
  }
  return 0
}

# Process a part through the workflows
proc process_part {part workflows workflow} {
  while {1} {
    if {$workflow eq "A"} {
      return 1
    } elseif {$workflow eq "R"} {
      return 0
    }

    set rules [dict get $workflows $workflow]
    foreach rule $rules {
      if {[llength $rule] == 1} {
        set workflow [lindex $rule 0]
        break
      } else {
        set category [lindex $rule 0]
        set operator [lindex $rule 1]
        set value [lindex $rule 2]
        set destination [lindex $rule 3]

        set part_value [dict get $part $category]
        switch $operator {
          ">" {
            if {$part_value > $value} {
              set workflow $destination
              break
            }
          }
          "<" {
            if {$part_value < $value} {
              set workflow $destination
              break
            }
          }
        }
      }
    }
  }
}

# Calculate the number of accepted combinations of ratings recursively
proc calculate_accepted_combinations {ranges workflows workflow} {
    if {$workflow eq "A"} {
        # Calculate the number of combinations within the ranges
        set combinations 1
        foreach category {x m a s} {
            set min [lindex [dict get $ranges $category] 0]
            set max [lindex [dict get $ranges $category] 1]
            set combinations [expr {$combinations * ($max - $min + 1)}]
        }
        return $combinations
    } elseif {$workflow eq "R"} {
        return 0
    }

    set rules [dict get $workflows $workflow]
    set total_combinations 0

    foreach rule $rules {
        if {[llength $rule] == 1} {
            # Unconditional rule
            set next_workflow [lindex $rule 0]
            set total_combinations [expr {$total_combinations + [calculate_accepted_combinations $ranges $workflows $next_workflow]}]
            break
        } else {
            set category [lindex $rule 0]
            set operator [lindex $rule 1]
            set value [lindex $rule 2]
            set destination [lindex $rule 3]

            set min [lindex [dict get $ranges $category] 0]
            set max [lindex [dict get $ranges $category] 1]

            if {$operator eq ">"} {
                # Create two ranges: one where the condition is met, and one where it's not
                if {$value >= $max} {
                    # Condition is never met
                    continue
                }

                set true_ranges [dict create {*}$ranges]
                dict set true_ranges $category [list [expr {$value + 1}] $max]
                set total_combinations [expr {$total_combinations + [calculate_accepted_combinations $true_ranges $workflows $destination]}]

                set false_ranges [dict create {*}$ranges]
                dict set false_ranges $category [list $min $value]
                set ranges $false_ranges ;# Update the ranges for the next rule
            } elseif {$operator eq "<"} {
                 if {$value <= $min} {
                    # Condition is never met
                    continue
                }
               set true_ranges [dict create {*}$ranges]
                dict set true_ranges $category [list $min [expr {$value - 1}]]
                set total_combinations [expr {$total_combinations + [calculate_accepted_combinations $true_ranges $workflows $destination]}]

                set false_ranges [dict create {*}$ranges]
                dict set false_ranges $category [list $value $max]
                set ranges $false_ranges ;# Update the ranges for the next rule
            }
        }
    }
    return $total_combinations
}

# Call the main function
exit [main]
