
#!/usr/bin/tclsh

proc main {} {
    set filename "input.txt"
    
    # Check if file exists before proceeding
    if {![file exists $filename]} {
        puts "Error: $filename not found."
        return
    }

    # Initialize a dictionary to store register values
    set registers [dict create]
    set fp [open $filename r]

    # Process instructions line by line
    while {[gets $fp line] >= 0} {
        set line [string trim $line]
        if {$line eq ""} continue

        # Format: reg op val "if" cond_reg cond_op cond_val
        # Example: b inc 5 if a > 1
        lassign $line reg op amount if_word cond_reg cond_op cond_val

        # Get current value of condition register (default to 0 if not yet seen)
        set val_cond [expr {[dict exists $registers $cond_reg] ? [dict get $registers $cond_reg] : 0}]

        # Evaluate the condition. 
        # Tcl's expr handles comparison operators (>, <, >=, <=, ==, !=) natively.
        if {[expr "$val_cond $cond_op $cond_val"]} {
            # Determine increment or decrement amount
            set delta [expr {$op eq "inc" ? $amount : -$amount}]
            
            # Use 'dict incr' to modify the register. 
            # It automatically initializes non-existent keys to 0.
            dict incr registers $reg $delta
        }
    }
    close $fp

    # Calculate the maximum value among all registers after processing
    set max_val 0
    # dict values returns a list of all values stored in the dictionary
    foreach v [dict values $registers] {
        if {$v > $max_val} {
            set max_val $v
        }
    }

    puts $max_val
}

# Run the program
main
