
#!/usr/bin/tclsh

# --- Pure Tcl MD5 Implementation (Minimalist) ---
# To avoid external libraries, we implement the MD5 message-digest algorithm.
proc md5 {msg} {
    set K {
        0xd76aa478 0xe8c7b756 0x242070db 0xc1bdceee 0xf57c0faf 0x4787c62a 0xa8304613 0xfd469501
        0x698098d8 0x8b44f7af 0xffff5bb1 0x895cd7be 0x6b901122 0xfd987193 0xa679438e 0x49b40821
        0xf61e2562 0xc040b340 0x265e5a51 0xe9b6c7aa 0xd62f105d 0x02441453 0xd8a1e681 0xe7d3fbc8
        0x21e1cde6 0xc33707d6 0xf4d50d87 0x455a14ed 0xa9e3e905 0xfcefa3f8 0x676f02d9 0x8d2a4c8a
        0xfffa3942 0x8771f681 0x6d9d6122 0xfde5380c 0xa4beea44 0x4bdecfa9 0xf6bb4b60 0xbebfbc70
        0x289b7ec6 0xeaa127fa 0xd4ef3085 0x04881d05 0xd9d4d039 0xe6db99e5 0x1fa27cf8 0xc4ac5665
        0xf4292244 0x432aff97 0xab9423a7 0xfc93a039 0x655b59c3 0x8f0ccc92 0xffeff47d 0x85845dd1
        0x6fa87e4f 0xfe2ce6e0 0xa3014314 0x4e0811a1 0xf7537e82 0xbd3af235 0x2ad7d2bb 0xeb86d391
    }
    set S {
        7 12 17 22  7 12 17 22  7 12 17 22  7 12 17 22
        5  9 14 20  5  9 14 20  5  9 14 20  5  9 14 20
        4 11 16 23  4 11 16 23  4 11 16 23  4 11 16 23
        6 10 15 21  6 10 15 21  6 10 15 21  6 10 15 21
    }

    # Pre-processing: Padding
    set bitLen [expr {[string length $msg] * 8}]
    append msg [binary format a*c "" 0x80]
    while {[expr {[string length $msg] % 64}] != 56} { append msg [binary format c 0] }
    append msg [binary format i2 [list $bitLen 0]]

    set h0 0x67452301; set h1 0xefcdab89; set h2 0x98badcfe; set h3 0x10325476

    # Process 512-bit chunks
    for {set i 0} {$i < [string length $msg]} {incr i 64} {
        binary scan [string range $msg $i [expr {$i+63}]] i16 M
        set a $h0; set b $h1; set c $h2; set d $h3
        for {set j 0} {$j < 64} {incr j} {
            if {$j < 16} {
                set f [expr {($b & $c) | ((~$b) & $d)}]
                set g $j
            } elseif {$j < 32} {
                set f [expr {($d & $b) | ((~$d) & $c)}]
                set g [expr {(5*$j + 1) % 16}]
            } elseif {$j < 48} {
                set f [expr {$b ^ $c ^ $d}]
                set g [expr {(3*$j + 5) % 16}]
            } else {
                set f [expr {$c ^ ($b | (~$d))}]
                set g [expr {(7*$j) % 16}]
            }
            set temp [expr {($a + $f + [lindex $K $j] + [lindex $M $g]) & 0xFFFFFFFF}]
            set rot [lindex $S $j]
            set a $d
            set d $c
            set c $b
            set b [expr {(($temp << $rot) | (($temp & 0xFFFFFFFF) >> (32-$rot))) + $b}]
        }
        set h0 [expr {($h0 + $a) & 0xFFFFFFFF}]
        set h1 [expr {($h1 + $b) & 0xFFFFFFFF}]
        set h2 [expr {($h2 + $c) & 0xFFFFFFFF}]
        set h3 [expr {($h3 + $d) & 0xFFFFFFFF}]
    }
    return [format "%08x%08x%08x%08x" \
        [expr {($h0<<24 | ($h0&0xFF00)<<8 | ($h0&0xFF0000)>>8 | $h0>>24) & 0xFFFFFFFF}] \
        [expr {($h1<<24 | ($h1&0xFF00)<<8 | ($h1&0xFF0000)>>8 | $h1>>24) & 0xFFFFFFFF}] \
        [expr {($h2<<24 | ($h2&0xFF00)<<8 | ($h2&0xFF0000)>>8 | $h2>>24) & 0xFFFFFFFF}] \
        [expr {($h3<<24 | ($h3&0xFF00)<<8 | ($h3&0xFF0000)>>8 | $h3>>24) & 0xFFFFFFFF}]]
}

# --- Maze Solving Logic ---
proc solve {passcode} {
    set queue [list [list 0 0 ""]]
    set shortest ""
    set longest 0
    
    set head 0
    while {$head < [llength $queue]} {
        lassign [lindex $queue $head] x y path
        incr head
        
        # Check if we reached the vault
        if {$x == 3 && $y == 3} {
            if {$shortest eq ""} { set shortest $path }
            set longest [expr {max($longest, [string length $path])}]
            continue
        }
        
        # Determine open doors based on hash
        set hash [md5 "$passcode$path"]
        set dirs {U D L R}
        set dx {0 0 -1 1}
        set dy {-1 1 0 0}
        
        for {set i 0} {$i < 4} {incr i} {
            set char [string index $hash $i]
            # Check if door is open (bcdef)
            if {[string first $char "bcdef"] != -1} {
                set nx [expr {$x + [lindex $dx $i]}]
                set ny [expr {$y + [lindex $dy $i]}]
                
                # Check grid boundaries
                if {$nx >= 0 && $nx < 4 && $ny >= 0 && $ny < 4} {
                    lappend queue [list $nx $ny "$path[lindex $dirs $i]"]
                }
            }
        }
    }
    
    return [list $shortest $longest]
}

# --- Main Entry Point ---
proc main {} {
    if {[catch {open "input.txt" r} fp]} {
        puts "Error: Could not open input.txt"
        return
    }
    set passcode [string trim [read $fp]]
    close $fp
    
    if {$passcode eq ""} { return }
    
    lassign [solve $passcode] shortest longest
    
    puts "Shortest Path: $shortest"
    puts "Longest Path Length: $longest"
}

main
