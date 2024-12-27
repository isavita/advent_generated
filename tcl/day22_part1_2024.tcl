
proc mix_and_prune {secret value} {
    set mixed [expr {$secret ^ $value}]
    return [expr {$mixed % 16777216}]
}

proc generate_next_secret {secret} {
    set secret [mix_and_prune $secret [expr {$secret * 64}]]
    set secret [mix_and_prune $secret [expr {int($secret / 32)}]]
    set secret [mix_and_prune $secret [expr {$secret * 2048}]]
    return $secret
}

proc solve {filename} {
    set total_sum 0
    set file [open $filename r]
    while {[gets $file line] != -1} {
        set secret $line
        for {set i 0} {$i < 2000} {incr i} {
            set secret [generate_next_secret $secret]
        }
        incr total_sum $secret
    }
    close $file
    puts $total_sum
}

solve "input.txt"
