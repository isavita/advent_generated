
# Function to parse and execute the instructions
proc execute_instructions {filename} {
    # Initialize registers a-h to 0
    array set registers {a 0 b 0 c 0 d 0 e 0 f 0 g 0 h 0}
    
    # Read the instructions from the file
    set file [open $filename r]
    set instructions [split [read $file] "\n"]
    close $file
    
    # Initialize the instruction pointer and mul count
    set ip 0
    set mul_count 0
    
    # Execute the instructions
    while {$ip < [llength $instructions]} {
        set instruction [lindex $instructions $ip]
        set opcode [lindex $instruction 0]
        set arg1 [lindex $instruction 1]
        set arg2 [lindex $instruction 2]
        
        # Determine the value of arg2 if it's a register
        if {[string is integer -strict $arg2]} {
            set value2 $arg2
        } else {
            set value2 $registers($arg2)
        }
        
        # Execute the instruction based on the opcode
        switch $opcode {
            "set" {
                set registers($arg1) $value2
            }
            "sub" {
                set registers($arg1) [expr {$registers($arg1) - $value2}]
            }
            "mul" {
                incr mul_count
                set registers($arg1) [expr {$registers($arg1) * $value2}]
            }
            "jnz" {
                if {[string is integer -strict $arg1]} {
                    set value1 $arg1
                } else {
                    set value1 $registers($arg1)
                }
                if {$value1 != 0} {
                    incr ip $value2
                    continue
                }
            }
        }
        
        # Increment the instruction pointer
        incr ip
    }
    
    # Return the count of mul instructions
    return $mul_count
}

# Call the function and print the result
puts [execute_instructions "input.txt"]
