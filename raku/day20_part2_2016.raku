
use v6;

sub MAIN() {
    my $max = 2**32 - 1;

    my @ranges = slurp('input.txt')
        .lines
        .map: {
            / ^ (\d+) '-' (\d+) $/;
            ($0.Int, $1.Int)
        };

    my @sorted = @ranges.sort: { $^a[0] <=> $^b[0] || $^a[1] <=> $^b[1] };

    my @merged;
    for @sorted -> $r {
        if @merged && @merged[*-1][1] >= $r[0] - 1 {
            @merged[*-1][1] = max @merged[*-1][1], $r[1];
        }
        else {
            @merged.push: [$r[0], $r[1]];
        }
    }

    if @merged && @merged[*-1][1] != $max {
        @merged.push: [$max, 0];
    }

    my $total = 0;
    for ^(@merged.elems - 1) -> $i {
        $total += @merged[$i+1][0] - @merged[$i][1] - 1;
    }

    say $total;
}
