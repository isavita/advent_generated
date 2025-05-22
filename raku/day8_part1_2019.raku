sub MAIN {
    my $data = 'input.txt'.IO.slurp.trim;

    my $width = 25;
    my $height = 6;
    my $layer-size = $width * $height;

    my $min-zeros = $layer-size + 1;
    my $result = 0;

    for $data.comb.rotor($layer-size, :partial) -> $layer-chars {
        my $bag = $layer-chars.Bag;

        my $zero-count = $bag{'0'} // 0;
        my $one-count  = $bag{'1'} // 0;
        my $two-count  = $bag{'2'} // 0;

        if $zero-count < $min-zeros {
            $min-zeros = $zero-count;
            $result = $one-count * $two-count;
        }
    }

    $result.say;
}