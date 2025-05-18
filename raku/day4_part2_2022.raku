
sub MAIN {
    say open("input.txt").lines.grep({
        my ($range1_str, $range2_str) = $_.split(',');
        my ($s1, $e1) = $range1_str.split('-').map({ .Int });
        my ($s2, $e2) = $range2_str.split('-').map({ .Int });
        $s1 <= $e2 and $e1 >= $s2;
    }).elems;
}
