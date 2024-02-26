
function bin2dec(bin) {
    dec = 0
    for (i = length(bin); i > 0; i--) {
        dec += (2 ^ (length(bin) - i)) * substr(bin, i, 1)
    }
    return dec
}

BEGIN {
    FS=""
}

{
    for(i=1; i<=NF; i++) {
        bit[i, $i]++
        total[i]++
    }
}

END {
    for(i=1; i in total; i++) {
        if(bit[i, 1] > bit[i, 0]) {
            gamma = gamma "1"
            epsilon = epsilon "0"
        } else {
            gamma = gamma "0"
            epsilon = epsilon "1"
        }
    }
    print "Gamma rate: " gamma
    print "Epsilon rate: " epsilon
    print "Power consumption: " bin2dec(gamma) * bin2dec(epsilon)
}
