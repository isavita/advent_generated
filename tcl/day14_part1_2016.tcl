package require md5
set CACHE_SIZE 2048
array set cache {}
for {set i 0} {$i<$CACHE_SIZE} {incr i} {
    set cache($i,index) -1
}
proc get_hash {salt index} {
    global CACHE_SIZE cache
    set idx [expr {$index % $CACHE_SIZE}]
    if {$cache($idx,index) == $index} {
        return $cache($idx,hash)
    }
    set hash [md5::md5 -hex "${salt}${index}"]
    set cache($idx,index) $index
    set cache($idx,hash) $hash
    return $hash
}
set f [open "input.txt"]
gets $f salt
close $f
set found 0
set idx 0
while {$found < 64} {
    set h [get_hash $salt $idx]
    set triplet {}
    for {set i 0} {$i<30} {incr i} {
        if {[string index $h $i] eq [string index $h [expr {$i+1}]] && [string index $h $i] eq [string index $h [expr {$i+2}]]} {
            set triplet [string index $h $i]
            break
        }
    }
    if {$triplet ne {}} {
        set quint [string repeat $triplet 5]
        for {set j 1} {$j<=1000} {incr j} {
            if {[string first $quint [get_hash $salt [expr {$idx+$j}]]] != -1} {
                incr found
                break
            }
        }
    }
    incr idx
}
puts [expr {$idx-1}]