
proc increment_password {password} {
    set len [string length $password]
    for {set i [expr {$len - 1}]} {$i >= 0} {incr i -1} {
        set char [string index $password $i]
        if {$char eq "z"} {
            set password [string replace $password $i $i "a"]
        } else {
            set password [string replace $password $i $i [format %c [expr {[scan $char %c] + 1}]]]
            break
        }
    }
    return $password
}

proc has_increasing_straight {password} {
    for {set i 0} {$i <= [expr {[string length $password] - 3}]} {incr i} {
        set a [scan [string index $password $i] %c]
        set b [scan [string index $password [expr {$i + 1}]] %c]
        set c [scan [string index $password [expr {$i + 2}]] %c]
        if {$b == $a + 1 && $c == $b + 1} {
            return 1
        }
    }
    return 0
}

proc has_invalid_chars {password} {
    return [expr {[string first "i" $password] != -1 || [string first "o" $password] != -1 || [string first "l" $password] != -1}]
}

proc has_two_pairs {password} {
    set pairs 0
    set last_pair ""
    for {set i 0} {$i <= [expr {[string length $password] - 2}]} {incr i} {
        set a [string index $password $i]
        set b [string index $password [expr {$i + 1}]]
        if {$a eq $b && $a ne $last_pair} {
            incr pairs
            set last_pair $a
        }
    }
    return [expr {$pairs >= 2}]
}

proc is_valid_password {password} {
    return [expr {[has_increasing_straight $password] && ![has_invalid_chars $password] && [has_two_pairs $password]}]
}

proc find_next_password {password} {
    while {1} {
        set password [increment_password $password]
        if {[is_valid_password $password]} {
            return $password
        }
    }
}

# Read the input password from the file
set file [open "input.txt" r]
set password [string trim [gets $file]]
close $file

# Find the next valid password
set next_password [find_next_password $password]
puts "Next valid password: $next_password"

# Find the next valid password after the first valid one
set next_password [find_next_password $next_password]
puts "Next valid password after that: $next_password"
