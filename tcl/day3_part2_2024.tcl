
proc main {} {
    set fp [open "input.txt" r]
    set c [read $fp]
    close $fp

    set e 1
    set s 0
    
    set p {(mul\([0-9]{1,3},[0-9]{1,3}\))|(do\(\))|(don't\(\))}

    foreach {m g1 g2 g3} [regexp -all -inline $p $c] {
        if {$g1 ne ""} {
            if {$e} {
                regexp {mul\((\d+),(\d+)\)} $g1 _ a b
                set r [expr {$a * $b}]
                incr s $r
                puts "Adding $a * $b = $r"
            }
        } elseif {$g2 ne ""} {
            set e 1
            puts "Multiplication enabled"
        } elseif {$g3 ne ""} {
            set e 0
            puts "Multiplication disabled"
        }
    }
    
    puts "Total sum: $s"
}

main
