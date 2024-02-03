
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my @lines = <$fh>;
close($fh);

my $ship = { x => 0, y => 0, facing => 0 };

foreach my $line (@lines) {
    my $action = substr($line, 0, 1);
    my $value = substr($line, 1);

    if ($action eq 'N') {
        $ship->{y} += $value;
    } elsif ($action eq 'S') {
        $ship->{y} -= $value;
    } elsif ($action eq 'E') {
        $ship->{x} += $value;
    } elsif ($action eq 'W') {
        $ship->{x} -= $value;
    } elsif ($action eq 'L') {
        $ship->{facing} = ($ship->{facing} - $value + 360) % 360;
    } elsif ($action eq 'R') {
        $ship->{facing} = ($ship->{facing} + $value) % 360;
    } elsif ($action eq 'F') {
        if ($ship->{facing} == 0) {
            $ship->{x} += $value;
        } elsif ($ship->{facing} == 90) {
            $ship->{y} -= $value;
        } elsif ($ship->{facing} == 180) {
            $ship->{x} -= $value;
        } elsif ($ship->{facing} == 270) {
            $ship->{y} += $value;
        }
    }
}

my $manhattanDistance = abs($ship->{x}) + abs($ship->{y});
print "$manhattanDistance\n";

sub abs {
    my $x = shift;
    if ($x < 0) {
        return -$x;
    }
    return $x;
}
