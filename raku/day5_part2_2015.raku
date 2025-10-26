
sub MAIN {
    my $nice = 0;
    for 'input.txt'.IO.lines -> $s {
        my $dup = False;
        my $gap = False;
        for 0..$s.chars-2 -> \i {
            $dup ||= $s.index($s.substr(i,2), i+2).defined;
            $gap ||= i+2 < $s.chars && $s.substr(i,1) eq $s.substr(i+2,1);
        }
        $nice++ if $dup && $gap;
    }
    say $nice;
}
