
#!/usr/bin/env tclsh

# Read whole file at once – faster than line-by-line
set fp [open input.txt r]
set data [read $fp]
close $fp

array set IngCount {}      ;# global ingredient -> occurrence counter
array set AllCand {}       ;# allergen -> list of possible ingredients
array set LineIng {}       ;# line -> list of ingredients (temporary)

set lineNo 0

# ---- first pass: build ingredient counts and initial candidate lists ----
foreach line [split $data \n] {
    incr lineNo
    set line [string trim $line]
    if {$line eq ""} continue

    # split into ingredients part and allergens part
    set idx [string first "(" $line]
    set ingPart [string trim [string range $line 0 [expr {$idx - 1}]]]
    set algPart [string trim [string range $line [expr {$idx + 9}] end-1]] ;# skip "contains " and final ")"

    # parse ingredients
    set LineIng($lineNo) $ingPart
    foreach ing $ingPart {
        incr IngCount($ing)
    }

    # parse allergens
    set allergens [split $algPart ,]
    foreach alg $allergens {
        set alg [string trim $alg]
        if {![info exists AllCand($alg)]} {
            # first time we see this allergen – all ingredients are candidates
            set AllCand($alg) $ingPart
        } else {
            # intersect candidates with current line
            set new {}
            foreach c $AllCand($alg) {
                if {$c in $ingPart} {lappend new $c}
            }
            set AllCand($alg) $new
        }
    }
}

# ---- Part 1: count safe ingredients ----
set safe 0
foreach ing [array names IngCount] {
    set danger 0
    foreach alg [array names AllCand] {
        if {$ing in $AllCand($alg)} {set danger 1; break}
    }
    if {!$danger} {incr safe $IngCount($ing)}
}
puts $safe

# ---- Part 2: resolve unique assignments ----
array set Assignment {}   ;# alg -> ingredient
while {[array size AllCand] > 0} {
    set changed 0
    foreach alg [array names AllCand] {
        if {[llength $AllCand($alg)] == 1} {
            set ing [lindex $AllCand($alg) 0]
            set Assignment($alg) $ing
            unset AllCand($alg)
            set changed 1
            # remove this ingredient from every other allergen
            foreach other [array names AllCand] {
                set idx [lsearch -exact $AllCand($other) $ing]
                if {$idx != -1} {
                    set AllCand($other) [lreplace $AllCand($other) $idx $idx]
                }
            }
            break
        }
    }
    if {!$changed} {error "cannot resolve allergens"}
}

# output sorted alphabetically by allergen name
set parts {}
foreach alg [lsort [array names Assignment]] {
    lappend parts $Assignment($alg)
}
puts [join $parts ,]
