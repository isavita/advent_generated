proc transform {subjectNumber loopSize} {
    set value 1
    for {set i 0} {$i < $loopSize} {incr i} {
        set value [expr {$value * $subjectNumber % 20201227}]
    }
    return $value
}

proc findLoopSize {publicKey} {
    set value 1
    set loopSize 0
    while {$value != $publicKey} {
        set value [expr {$value * 7 % 20201227}]
        incr loopSize
    }
    return $loopSize
}

set file [open "input.txt" r]
set cardPublicKey [gets $file]
set doorPublicKey [gets $file]
close $file

set cardLoopSize [findLoopSize $cardPublicKey]
set encryptionKey [transform $doorPublicKey $cardLoopSize]

puts $encryptionKey