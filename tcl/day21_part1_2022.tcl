
#!/usr/bin/env tclsh

# Parses the input and returns a dictionary of monkey jobs.
proc parse_input {filename} {
    set monkey_jobs {}
    set file [open $filename r]
    while {[gets $file line] != -1} {
        regexp {^(\w+): (.*)$} $line _ monkey job
        dict set monkey_jobs $monkey $job
    }
    close $file
    return $monkey_jobs
}

# Evaluates the job of a monkey.
proc evaluate_job {monkey_jobs monkey memo} {
    if {[dict exists $memo $monkey]} {
        return [dict get $memo $monkey]
    }

    set job [dict get $monkey_jobs $monkey]
    if {[string is integer $job]} {
        dict set memo $monkey $job
        return $job
    } else {
        regexp {^(\w+) ([+\-*/]) (\w+)$} $job _ monkey1 op monkey2
        set val1 [evaluate_job $monkey_jobs $monkey1 $memo]
        set val2 [evaluate_job $monkey_jobs $monkey2 $memo]

        switch -- $op {
            "+" {set result [expr {$val1 + $val2}]}
            "-" {set result [expr {$val1 - $val2}]}
            "*" {set result [expr {$val1 * $val2}]}
            "/" {set result [expr {$val1 / $val2}]}
            default {puts "Error: Unknown operation $op"; exit 1}
        }

        dict set memo $monkey $result
        return $result
    }
}


# Main program
set filename "input.txt"
set monkey_jobs [parse_input $filename]
set memo {}
set root_value [evaluate_job $monkey_jobs root $memo]

puts $root_value
