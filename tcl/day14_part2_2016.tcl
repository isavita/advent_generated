package require md5

set salt [string trim [read [set f [open "input.txt" r]]]]
close $f

# Split cache into two flat arrays — compound keys like "$slot,index" are slower
set CACHE_SIZE 4096
for {set i 0} {$i < $CACHE_SIZE} {incr i} {
    set cache_idx($i) -1
    set cache_hash($i) ""
}

proc stretched_hash {salt index} {
    global CACHE_SIZE cache_idx cache_hash
    set slot [expr {$index % $CACHE_SIZE}]
    if {$cache_idx($slot) == $index} {
        return $cache_hash($slot)
    }
    # string tolower for AoC correctness — md5 hex case varies by platform
    set hash [string tolower [md5::md5 -hex "${salt}${index}"]]
    for {set i 0} {$i < 2016} {incr i} {
        set hash [string tolower [md5::md5 -hex $hash]]
    }
    set cache_idx($slot) $index
    set cache_hash($slot) $hash
    return $hash
}

set found 0
set index 0
while {$found < 64} {
    set h [stretched_hash $salt $index]
    # regexp is much faster than a char-by-char Tcl loop
    if {[regexp {(.)\1\1} $h -> c]} {
        set quint "$c$c$c$c$c"
        # Direct loop variable avoids repeated [expr {$index + $j}]
        set jstart [expr {$index + 1}]
        set jend   [expr {$index + 1000}]
        for {set j $jstart} {$j <= $jend} {incr j} {
            if {[string first $quint [stretched_hash $salt $j]] != -1} {
                incr found
                break
            }
        }
    }
    incr index
}
puts [expr {$index - 1}]
