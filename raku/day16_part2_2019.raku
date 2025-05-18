
sub MAIN {
    my $input_string = 'input.txt'.IO.slurp.trim;
    my $L_orig = $input_string.chars;
    my $offset = $input_string.substr(0, 7).Int;

    my $tail_len = $L_orig * 10000 - $offset;

    my @digits = (0 .. $tail_len - 1).map: {
        ($input_string.substr(($offset + $_) % $L_orig, 1)).Int
    };

    for 1 .. 100 -> $phase {
        my $total = 0;
        for (@digits.end ... 0) -> $i {
            $total += @digits[$i];
            @digits[$i] = $total % 10;
        }
    }

    say @digits[0..7].join;
}
