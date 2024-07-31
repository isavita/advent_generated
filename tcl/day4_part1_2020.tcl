set file [open "input.txt" r]
set passports {}
set passport ""

while {[gets $file line] >= 0} {
    if {$line eq ""} {
        lappend passports $passport
        set passport ""
    } else {
        append passport " $line"
    }
}
if {$passport ne ""} {
    lappend passports $passport
}
close $file

set validPassports 0
set requiredFields {byr iyr eyr hgt hcl ecl pid}

proc isValid {passport requiredFields} {
    foreach field $requiredFields {
        if {[string first "$field:" $passport] == -1} {
            return 0
        }
    }
    return 1
}

foreach p $passports {
    if {[isValid $p $requiredFields]} {
        incr validPassports
    }
}

puts $validPassports