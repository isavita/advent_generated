
sub MAIN {
    my ($card-public-key, $door-public-key) = 'input.txt'.IO.lines.map(*.Int);
    my constant MOD = 20201227;

    sub find-loop-size($public-key) {
        my $value = 1;
        my $subject-number = 7;
        my $loop-size = 0;
        loop {
            $loop-size++;
            $value = ($value * $subject-number) % MOD;
            last if $value == $public-key;
        }
        $loop-size
    }

    my $card-loop-size = find-loop-size($card-public-key);

    say $door-public-key ** $card-loop-size % MOD;
}
