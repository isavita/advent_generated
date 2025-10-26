
sub find-digits(Str $_ --> Int) {
    my @digits = <zero one two three four five six seven eight nine>;
    my $first = 0;
    my $last  = 0;

    for ^.chars -> \i {
        when .substr(i,1).match(/\d/) {
            my \d = .substr(i,1).Int;
            $first ||= d;
            $last   = d;
        }
        for @digits.kv -> \j, \name {
            if .substr(i).starts-with(name) {
                $first ||= j;
                $last   = j;
                last;
            }
        }
    }
    10 * $first + $last
}

say (slurp 'input.txt').lines.map(&find-digits).sum
