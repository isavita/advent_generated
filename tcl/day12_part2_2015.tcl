
#!/usr/bin/tclsh

# Read input file
set fp [open "input.txt" r]
set data [read $fp]
close $fp

# Part 1: Sum all numbers in the file
set sum1 0
foreach num [regexp -inline -all -- {-?\d+} $data] {
    incr sum1 $num
}
puts "Part 1 Sum: $sum1"

# Part 2: Sum numbers ignoring objects with "red" values
proc sum_json {str_name pos_name} {
    upvar 1 $str_name str
    upvar 1 $pos_name pos
    
    # Skip whitespace
    while {[string is space -strict [string index $str $pos]]} { incr pos }

    set char [string index $str $pos]

    # Object
    if {$char eq "\{"} {
        incr pos
        set sum 0
        set is_red 0
        
        # Look ahead for "red" value while parsing
        while 1 {
            while {[string is space -strict [string index $str $pos]]} { incr pos }
            if {[string index $str $pos] eq "\}"} { incr pos; break }

            # Parse Key
            if {[string index $str $pos] eq "\""} {
                 incr pos
                 while {[string index $str $pos] ne "\""} { 
                     if {[string index $str $pos] eq "\\"} { incr pos }
                     incr pos 
                 }
                 incr pos
            }

            # Parse Colon
            while {[string is space -strict [string index $str $pos]]} { incr pos }
            if {[string index $str $pos] eq ":"} { incr pos }
            
            # Check Value for "red" string
            set temp_pos $pos
            while {[string is space -strict [string index $str $temp_pos]]} { incr temp_pos }
            if {[string range $str $temp_pos $temp_pos+4] eq {"red"}} {
                set is_red 1
            }

            # Parse Value recursively
            set val [sum_json str pos]
            
            # Only accumulate if not red
            if {!$is_red} { incr sum $val }

            # Skip comma
            while {[string is space -strict [string index $str $pos]]} { incr pos }
            if {[string index $str $pos] eq ","} { incr pos }
        }
        
        return [expr {$is_red ? 0 : $sum}]
    }

    # Array
    if {$char eq "\["} {
        incr pos
        set sum 0
        while 1 {
            while {[string is space -strict [string index $str $pos]]} { incr pos }
            if {[string index $str $pos] eq "\]"} { incr pos; break }
            
            incr sum [sum_json str pos]
            
            while {[string is space -strict [string index $str $pos]]} { incr pos }
            if {[string index $str $pos] eq ","} { incr pos }
        }
        return $sum
    }

    # String
    if {$char eq "\""} {
        incr pos
        while {[string index $str $pos] ne "\""} {
            if {[string index $str $pos] eq "\\"} { incr pos }
            incr pos
        }
        incr pos
        return 0
    }

    # Number
    if {$char eq "-" || [string is digit -strict $char]} {
        set start $pos
        if {$char eq "-"} { incr pos }
        while {[string is digit -strict [string index $str $pos]]} { incr pos }
        return [string range $str $start [expr {$pos - 1}]]
    }

    # Literal (true, false, null)
    while {[string is alpha -strict [string index $str $pos]]} { incr pos }
    return 0
}

set pos 0
puts "Part 2 Sum: [sum_json data pos]"
