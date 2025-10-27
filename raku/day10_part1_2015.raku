
sub MAIN {
    my $seq = slurp 'input.txt';
    $seq .= trim;
    for ^40 { $seq = next-seq($seq) }
    say $seq.chars;
}

sub next-seq(Str $s) {
    my @parts;
    my $i = 0;
    while $i < $s.chars {
        my $digit = $s.substr: $i, 1;
        my $count = 1;
        while $i + $count < $s.chars && $s.substr($i + $count, 1) eq $digit {
            $count++;
        }
        @parts.push: $count ~ $digit;
        $i += $count;
    }
    @parts.join;
}
