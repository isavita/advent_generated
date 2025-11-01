
sub MAIN {
    my @nums = 'input.txt'.IO.lines».Int;
    my $N = +@nums;
    my @idx = ^$N;
    my @val = @nums.map: * * 811589153;

    for ^10 {
        for ^$N -> $i {
            my $pos = @idx.first($i, :k);
            my $v = @val[$pos];
            next unless $v;
            my $tgt = ($pos + $v) % ($N - 1);
            @val.splice($pos, 1);
            @val.splice($tgt, 0, $v);
            @idx.splice($pos, 1);
            @idx.splice($tgt, 0, $i);
        }
    }

    my $zero = @val.first(0, :k);
    say [+] @val[($zero + 1000, $zero + 2000, $zero + 3000) >>%>> $N];
}
