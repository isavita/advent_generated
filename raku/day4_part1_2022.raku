
sub MAIN {
    my $count = 'input.txt'.IO.lines.grep( -> $line {
        my ($r1, $r2) = $line.split(',');
        my ($s1, $e1) = $r1.split('-').map(*.Int);
        my ($s2, $e2) = $r2.split('-').map(*.Int);
        ($s1 <= $s2 && $e1 >= $e2) || ($s2 <= $s1 && $e2 >= $e1)
    }).elems;
    say $count;
}

