
#!/usr/bin/tclsh

proc solve {filename steps} {
  set fp [open $filename r]
  set template [string trim [gets $fp]]
  gets $fp ;# Skip the empty line

  set rules {}
  while {[gets $fp line] >= 0} {
    if {[regexp {([A-Z]+) -> ([A-Z]+)} $line _ pair insert]} {
      dict set rules $pair $insert
    }
  }
  close $fp

  # Efficient pair counting approach
  set pairs {}
  for {set i 0} {$i < [string length $template] - 1} {incr i} {
    set pair [string range $template $i [expr {$i + 1}]]
    if {[dict exists $pairs $pair]} {
      dict incr pairs $pair
    } else {
      dict set pairs $pair 1
    }
  }

  for {set step 0} {$step < $steps} {incr step} {
    set new_pairs {}
    dict for {pair count} $pairs {
      if {[dict exists $rules $pair]} {
        set insert [dict get $rules $pair]
        set first_pair [string index $pair 0]$insert
        set second_pair $insert[string index $pair 1]

        if {[dict exists $new_pairs $first_pair]} {
          dict incr new_pairs $first_pair $count
        } else {
          dict set new_pairs $first_pair $count
        }
        if {[dict exists $new_pairs $second_pair]} {
          dict incr new_pairs $second_pair $count
        } else {
          dict set new_pairs $second_pair $count
        }
      } else {
        if {[dict exists $new_pairs $pair]} {
          dict incr new_pairs $pair $count
        } else {
          dict set $new_pairs $pair $count
        }
      }
    }
    set pairs $new_pairs
  }

  # Count individual elements based on pairs
  set element_counts {}
  foreach {pair count} [dict get $pairs] {
    foreach char [split $pair ""] {
      if {[dict exists $element_counts $char]} {
        dict incr element_counts $char $count
      } else {
        dict set element_counts $char $count
      }
    }
  }

  # Adjust counts (account for double counting)
  dict for {element count} $element_counts {
    dict set element_counts $element [expr {($count + 1) / 2.0}]
  }

  # Find min and max element counts
  set min_count INF
  set max_count 0
  dict for {element count} $element_counts {
    if {$count < $min_count} {
      set min_count $count
    }
    if {$count > $max_count} {
      set max_count $count
    }
  }

  return [expr {$max_count - $min_count}]
}


# Main entry point
if {[info script] eq $argv0} {
  set result [solve "input.txt" 10]
  puts "Result for part 1: $result"
}
