
sub hash-string(Str $s) {
    my $res = 0;
    for $s.comb -> $c {
        $res += $c.ord;
        $res = ($res * 17) % 256;
    }
    $res
}

sub MAIN {
    my $text = slurp 'input.txt';
    my @tokens = $text.trim.split(',');
    my $total = 0;
    for @tokens -> $tok {
        $total += hash-string($tok);
    }
    say $total;
}
