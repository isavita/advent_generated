set file [open "input.txt" r]
set data [read $file]
close $file

set passphrases [split $data "\n"]
set validCount 0

proc sortString {w} {
    set chars [split $w ""]
    set sorted [lsort $chars]
    return [join $sorted ""]
}

foreach passphrase $passphrases {
    set words [split $passphrase " "]
    set wordSet [dict create]
    set valid 1

    foreach word $words {
        set sortedWord [sortString $word]
        if {[dict exists $wordSet $sortedWord]} {
            set valid 0
            break
        }
        dict set wordSet $sortedWord 1
    }

    if {$valid} {
        incr validCount
    }
}

puts $validCount