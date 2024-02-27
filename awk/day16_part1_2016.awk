
{
    a = $0;
    while (length(a) < 272) {
        b = "";
        for (i = length(a); i > 0; i--) {
            if (substr(a, i, 1) == "0") {
                b = b "1";
            } else {
                b = b "0";
            }
        }
        a = a "0" b;
    }
    a = substr(a, 1, 272);
    while (length(a) % 2 == 0) {
        checksum = "";
        for (i = 1; i <= length(a); i += 2) {
            if (substr(a, i, 1) == substr(a, i + 1, 1)) {
                checksum = checksum "1";
            } else {
                checksum = checksum "0";
            }
        }
        a = checksum;
    }
    print a;
}
