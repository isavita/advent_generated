
#!/usr/bin/env tclsh

proc calculate_mul_sum {filename} {
    set total_sum 0
    if {[catch {open $filename r} file_handle]} {
        puts "Error opening file: $filename"
        return -1
    }
    
    while {[gets $file_handle line] != -1} {
        # Use a regular expression to find valid mul instructions
        foreach {match x y} [regexp -all -inline {mul\((\d{1,3}),(\d{1,3})\)} $line] {
            set total_sum [expr {$total_sum + $x * $y}]
        }
    }
    close $file_handle
    return $total_sum
}

# Main execution
set filename "input.txt"
set result [calculate_mul_sum $filename]

if {$result >= 0} {
    puts $result
}
