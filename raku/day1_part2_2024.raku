
use v6;

sub MAIN {
    my @data = 'input.txt'.IO.lines.map: { $_.split(/\s+/).map(*.Int) };
    my @ids = @data.map: { $_[0] };
    my @similarities = @data.map: { $_[1] };
    my $sim-counts = @similarities.Bag;
    say @ids.map({ $_ * $sim-counts{$_} }).sum;
}
