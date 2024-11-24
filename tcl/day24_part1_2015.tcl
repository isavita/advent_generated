
proc read_weights {filename} {
    set fh [open $filename r]
    set weights [split [read $fh] "\n"]
    close $fh
    return [lsort -integer -decreasing $weights]
}

proc quantum_entanglement {group} {
    set qe 1
    foreach weight $group {
        set qe [expr {$qe * $weight}]
    }
    return $qe
}

proc find_partitions {weights target_weight} {
    set partitions {}
    set n [llength $weights]
    set max_group_size [expr {$n / 3}]
    
    for {set i 1} {$i <= $max_group_size} {incr i} {
        set groups [combinations $weights $i]
        foreach group $groups {
            if {[lsum $group] == $target_weight} {
                lappend partitions $group
            }
        }
        if {[llength $partitions] > 0} {
            break
        }
    }
    return $partitions
}

proc combinations {list n} {
    if {$n == 0} {
        return [list {}]
    }
    set result {}
    set len [llength $list]
    for {set i 0} {$i <= $len - $n} {incr i} {
        set first [lindex $list $i]
        set rest [lrange $list [expr {$i + 1}] end]
        foreach comb [combinations $rest [expr {$n - 1}]] {
            lappend result [linsert $comb 0 $first]
        }
    }
    return $result
}

proc lsum {list} {
    set sum 0
    foreach item $list {
        set sum [expr {$sum + $item}]
    }
    return $sum
}

proc main {} {
    set weights [read_weights "input.txt"]
    set total_weight [lsum $weights]
    set target_weight [expr {$total_weight / 3}]
    
    set partitions [find_partitions $weights $target_weight]
    set min_qe -1
    
    foreach partition $partitions {
        set qe [quantum_entanglement $partition]
        if {$min_qe == -1 || $qe < $min_qe} {
            set min_qe $qe
        }
    }
    
    puts "The quantum entanglement of the ideal configuration is: $min_qe"
}

main
