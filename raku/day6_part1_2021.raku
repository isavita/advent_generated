
sub MAIN() {
    my @fishes = 0 xx 9;
    "input.txt".IO.slurp.split(',').map({ @fishes[$_]++ });

    for ^80 {
        my $new = @fishes.shift;
        @fishes[6] += $new;
        @fishes.push: $new;
    }

    say @fishes.sum;
}
