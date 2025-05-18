
sub find-steps (Int $num) {
    if $num == 1 { return 0 }
    my $r = $num.Num.sqrt.ceiling;
    my $s = ($r % 2 == 0) ?? $r + 1 !! $r;
    my $k = ($s - 1) div 2;
    $k + abs(($s * $s - $num) % ($s - 1) - $k);
}

sub MAIN {
    my $num = 'input.txt'.IO.slurp.Int;
    say find-steps($num);
}

