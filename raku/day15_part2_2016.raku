
#!/usr/bin/env raku

sub MAIN {
    my $fh = "input.txt".IO.open or die "Cannot open file: $!";
    my @discs;
    for $fh.lines -> $line {
        if $line ~~ / 'Disc #' \d+ ' has ' (\d+) ' positions; at time=0, it is at position ' (\d+) '.' / {
            @discs.push: { positions => $0.Int, start => $1.Int };
        }
    }
    $fh.close;
    @discs.push: { positions => 11, start => 0 };
    say solve(@discs);
}

sub solve(@discs) {
    my $a = 0;
    my $m = 1;
    for @discs.kv -> $i, $d {
        my $mod = $d<positions>;
        my $off = ($d<start> + $i + 1) % $mod;
        my $r = ($mod - $off) % $mod;   # desired residue
        my $g = gcd($m, $mod);
        return -1 if ($r - $a) % $g != 0;   # should not happen
        my $l = ($m div $g) * $mod;
        my $inv = modinv($m div $g, $mod div $g);
        my $k = ((($r - $a) div $g) * $inv) % ($mod div $g);
        $a = ($a + $m * $k) % $l;
        $m = $l;
    }
    $a;
}

sub gcd($a, $b) { $b == 0 ?? $a !! gcd($b, $a % $b) }

sub egcd($a, $b) {
    return ($a, 1, 0) if $b == 0;
    my ($g, $x1, $y1) = egcd($b, $a % $b);
    return ($g, $y1, $x1 - ($a div $b) * $y1);
}

sub modinv($a, $m) {
    my ($g, $x, $y) = egcd($a, $m);
    die "modinv: no inverse" unless $g == 1;
    $x % $m;
}
