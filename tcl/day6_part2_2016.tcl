
#!/usr/bin/env tclsh

# Function to find the most/least frequent character in a column
proc find_char {column_data mode} {
    array set counts {}
    foreach char $column_data {
        if {[info exists counts($char)]} {
            incr counts($char)
        } else {
            set counts($char) 1
        }
    }

    set best_char ""
    set best_count [expr {$mode eq "most" ? -1 : [llength $column_data] + 1}]

    foreach {char count} [array get counts] {
        if {$mode eq "most"} {
            if {$count > $best_count} {
                set best_count $count
                set best_char $char
            }
        } else {
            if {$count < $best_count} {
                set best_count $count
                set best_char $char
            }
        }
    }
    return $best_char
}

# Function to process the input and return the decoded message
proc decode_message {filename mode} {
    set file [open $filename r]
    set lines [split [read $file] "\n"]
    close $file

    if {[llength $lines] == 0} {
        return ""
    }

    set num_cols [string length [lindex $lines 0]]
    set decoded_message ""

    for {set col 0} {$col < $num_cols} {incr col} {
        set column_data {}
        foreach line $lines {
            if {[string length $line] > $col} {
                lappend column_data [string index $line $col]
            }
        }
        append decoded_message [find_char $column_data $mode]
    }
    return $decoded_message
}

# Main program
if {[file exists "input.txt"]} {
    puts "Part 1: [decode_message "input.txt" "most"]"
    puts "Part 2: [decode_message "input.txt" "least"]"
} else {
    puts "Error: input.txt not found."
}
