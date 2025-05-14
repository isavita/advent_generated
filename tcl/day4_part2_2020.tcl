
proc validateYear {value min max} {
    if {![regexp {^\d{4}$} $value]} {
        return 0
    }
    if {[catch {set intYear [expr {int($value)}]} errmsg]} {
        return 0
    }
    if {$intYear >= $min && $intYear <= $max} {
        return 1
    }
    return 0
}

proc validateByr {value} {
    validateYear $value 1920 2002
}

proc validateIyr {value} {
    validateYear $value 2010 2020
}

proc validateEyr {value} {
    validateYear $value 2020 2030
}

proc validateHgt {value} {
    if {[regexp {^(\d+)(cm|in)$} $value -> heightStr unit]} {
        if {[catch {set height [expr {int($heightStr)}]} errmsg]} {
            return 0
        }
        switch $unit {
            "cm" {
                if {$height >= 150 && $height <= 193} {
                    return 1
                }
            }
            "in" {
                if {$height >= 59 && $height <= 76} {
                    return 1
                }
            }
        }
    }
    return 0
}

proc validateHcl {value} {
    regexp {^#[0-9a-f]{6}$} $value
}

proc validateEcl {value} {
    set allowedColors {"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
    if {[lsearch -exact $allowedColors $value] != -1} {
        return 1
    }
    return 0
}

proc validatePid {value} {
    regexp {^\d{9}$} $value
}

proc isValidPassport {passportDict} {
    set requiredFields {byr iyr eyr hgt hcl ecl pid}
    foreach field $requiredFields {
        if {![dict exists $passportDict $field]} {
            return 0
        }
    }

    if {![validateByr [dict get $passportDict byr]]} { return 0 }
    if {![validateIyr [dict get $passportDict iyr]]} { return 0 }
    if {![validateEyr [dict get $passportDict eyr]]} { return 0 }
    if {![validateHgt [dict get $passportDict hgt]]} { return 0 }
    if {![validateHcl [dict get $passportDict hcl]]} { return 0 }
    if {![validateEcl [dict get $passportDict ecl]]} { return 0 }
    if {![validatePid [dict get $passportDict pid]]} { return 0 }

    return 1
}

if {[file exists "input.txt"]} {
    set fid [open "input.txt" r]
    set validPassports 0
    set currentPassportString ""

    while {[gets $fid line] >= 0} {
        set trimmedLine [string trim $line]
        if {$trimmedLine eq ""} {
            if {[string length $currentPassportString] > 0} {
                set fields [split [string trim $currentPassportString]]
                set passportDict [dict create]
                foreach fieldPair $fields {
                    set parts [split $fieldPair ":"]
                    if {[llength $parts] == 2} {
                        dict set passportDict [lindex $parts 0] [lindex $parts 1]
                    }
                }
                if {[isValidPassport $passportDict]} {
                    incr validPassports
                }
            }
            set currentPassportString ""
        } else {
            if {[string length $currentPassportString] > 0} {
                append currentPassportString " "
            }
            append currentPassportString $trimmedLine
        }
    }

    if {[string length $currentPassportString] > 0} {
        set fields [split [string trim $currentPassportString]]
        set passportDict [dict create]
        foreach fieldPair $fields {
            set parts [split $fieldPair ":"]
            if {[llength $parts] == 2} {
                dict set passportDict [lindex $parts 0] [lindex $parts 1]
            }
        }
        if {[isValidPassport $passportDict]} {
            incr validPassports
        }
    }

    close $fid
    puts $validPassports
} else {
    puts stderr "Error: input.txt not found"
    exit 1
}
