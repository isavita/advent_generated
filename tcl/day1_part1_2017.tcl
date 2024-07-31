proc calculate_captcha {filename} {
    set file [open $filename r]
    set digits [read $file]
    close $file

    set sum 0
    set len [string length $digits]

    for {set i 0} {$i < $len} {incr i} {
        set current [string index $digits $i]
        set next [string index $digits [expr {($i + 1) % $len}]]
        if {$current == $next} {
            set sum [expr {$sum + $current}]
        }
    }

    return $sum
}

puts [calculate_captcha "input.txt"]