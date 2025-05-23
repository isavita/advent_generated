
sub MAIN {
    my constant $TOTAL_CUPS = 1000000;
    my constant $TOTAL_MOVES = 10000000;

    my $input = 'input.txt'.IO.slurp.trim;

    my @cups;
    @cups[1 .. $TOTAL_CUPS] = 0;

    my $first-cup-input = $input.substr(0,1).Int;
    my $prev-cup = $first-cup-input;

    for $input.comb.skip(1) -> $char {
        @cups[$prev-cup] = $char.Int;
        $prev-cup = $char.Int;
    }

    for $input.chars + 1 ... $TOTAL_CUPS -> $i {
        @cups[$prev-cup] = $i;
        $prev-cup = $i;
    }

    @cups[$prev-cup] = $first-cup-input;

    my $current-cup = $first-cup-input;

    for ^$TOTAL_MOVES -> $move-idx {
        my $pickup1 = @cups[$current-cup];
        my $pickup2 = @cups[$pickup1];
        my $pickup3 = @cups[$pickup2];

        @cups[$current-cup] = @cups[$pickup3];

        my $destination-cup = $current-cup - 1;
        if $destination-cup == 0 {
            $destination-cup = $TOTAL_CUPS;
        }

        while $destination-cup == $pickup1 || $destination-cup == $pickup2 || $destination-cup == $pickup3 {
            $destination-cup--;
            if $destination-cup == 0 {
                $destination-cup = $TOTAL_CUPS;
            }
        }

        @cups[$pickup3] = @cups[$destination-cup];
        @cups[$destination-cup] = $pickup1;

        $current-cup = @cups[$current-cup];
    }

    my $cup1 = @cups[1];
    my $cup2 = @cups[$cup1];
    say $cup1 * $cup2;
}
