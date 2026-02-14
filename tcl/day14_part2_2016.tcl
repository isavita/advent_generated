package require md5

set CACHE_SIZE 4096
array set cache {}
for {set i 0} {$i < $CACHE_SIZE} {incr i} {
    set cache($i,index) -1
}

proc stretched_hash {salt index} {
    global CACHE_SIZE cache
    set slot [expr {$index % $CACHE_SIZE}]
    if {$cache($slot,index) == $index} {
        return $cache($slot,hash)
    }

    set hash [md5::md5 -hex "${salt}${index}"]
    for {set i 0} {$i < 2016} {incr i} {
        set hash [md5::md5 -hex $hash]
    }

    set cache($slot,index) $index
    set cache($slot,hash) $hash
    return $hash
}

proc first_triplet {hash} {
    for {set i 0} {$i <= [expr {[string length $hash] - 3}]} {incr i} {
        set c [string index $hash $i]
        if {$c eq [string index $hash [expr {$i + 1}]] && $c eq [string index $hash [expr {$i + 2}]]} {
            return $c
        }
    }
    return ""
}

set f [open "input.txt" r]
set salt [string trim [read $f]]
close $f

set found 0
set index 0
while {$found < 64} {
    set h [stretched_hash $salt $index]
    set trip [first_triplet $h]
    if {$trip ne ""} {
        set quint [string repeat $trip 5]
        for {set j 1} {$j <= 1000} {incr j} {
            if {[string first $quint [stretched_hash $salt [expr {$index + $j}]]] != -1} {
                incr found
                break
            }
        }
    }
    incr index
}

puts [expr {$index - 1}]
