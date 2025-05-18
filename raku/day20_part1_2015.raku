
use v6;

sub MAIN {
    my $puzzle-input = 'input.txt'.IO.slurp.Int;
    my $Limit = $puzzle-input div 8 + 1;
    my @sums = (0) xx ($Limit + 1);

    for 1 .. $Limit -> $factor {
        for $factor, $factor * 2 ... $Limit -> $multiple {
            @sums[$multiple] += $factor;
        }
    }

    for 1 .. $Limit -> $house {
        if @sums[$house] * 10 >= $puzzle-input {
            say $house;
            exit;
        }
    }
}
