
sub fuel-cost($diff) {
    $diff * ($diff + 1) div 2
}

sub total-fuel(@positions, $target) {
    sum @positions.map: { fuel-cost( ($_ - $target).abs ) }
}

sub MAIN() {
    my @positions = 'input.txt'.IO.slurp.split(',').map: *.Int;
    my $min-pos = @positions.min;
    my $max-pos = @positions.max;

    my $min-fuel = min ($min-pos .. $max-pos).map: { total-fuel(@positions, $_) };

    say $min-fuel;
}

