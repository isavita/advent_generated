
# solution.awk

BEGIN {
    FS="";
    OFS="";
}

{
    originalLength = NF;
    encoded = "\"";

    for (i = 1; i <= NF; i++) {
        ch = $i;

        if (ch == "\\" || ch == "\"") {
            encoded = encoded "\\";
        }

        encoded = encoded ch;
    }

    encoded = encoded "\"";
    encodedLength = length(encoded);
    totalDiff += encodedLength - originalLength;
}

END {
    print totalDiff;
}
