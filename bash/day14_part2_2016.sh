
perl -MDigest::MD5=md5_hex -e '
$s = <>; $s =~ s/\s+$//;
sub h {
    $i = shift;
    $c{$i} //= do {
        $v = md5_hex($s . $i);
        $v = md5_hex($v) for 1..2016;
        $v
    };
}
while ($k < 64) {
    if (h($n) =~ /(.)\1\1/) {
        $q = $1 x 5;
        for $j (1..1000) {
            if (h($n + $j) =~ /\Q$q/) {
                $k++;
                last;
            }
        }
    }
    if ($k == 64) { print $n; exit; }
    $n++;
}
' input.txt
