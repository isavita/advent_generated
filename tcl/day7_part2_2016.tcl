
proc findABAs {s} {
    set abas {}
    for {set i 0} {$i < [string length $s] - 2} {incr i} {
        if {[string index $s $i] ne [string index $s [expr {$i + 1}]] && [string index $s $i] eq [string index $s [expr {$i + 2}]]} {
            lappend abas [string range $s $i [expr {$i + 2}]]
        }
    }
    return $abas
}

proc supportsSSL {ip} {
    set bracketContents [regexp -all -inline {\[[a-z]+\]} $ip]
    set ip [regsub -all {\[[a-z]+\]} $ip "-"]
    foreach aba [findABAs $ip] {
        set bab [string range $aba 1 1][string range $aba 0 0][string range $aba 1 1]
        foreach bracketContent $bracketContents {
            if {[string first $bab $bracketContent] != -1} {
                return 1
            }
        }
    }
    return 0
}

set sslCount 0
if {[catch {open "input.txt" r} file]} {
    puts "Error opening file: $file"
    exit 1
}
while {[gets $file line] >= 0} {
    if {[supportsSSL $line]} {
        incr sslCount
    }
}
close $file
puts $sslCount
