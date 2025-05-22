
sub MAIN {
    my @lines = 'input.txt'.IO.lines;

    my @slopes = (
        [1, 1],
        [3, 1],
        [5, 1],
        [7, 1],
        [1, 2],
    );

    my $product = 1;

    for @slopes -> @slope {
        my ($right-step, $down-step) = @slope;

        my $tree-count = 0;
        my $pos = 0;

        for @lines[0, $down-step ...^ *] -> $line {
            if $line.substr($pos, 1) eq '#' {
                $tree-count++;
            }
            $pos = ($pos + $right-step) % $line.chars;
        }
        $product *= $tree-count;
    }

    say $product;
}
