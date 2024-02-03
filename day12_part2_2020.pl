
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my @lines = <$fh>;
close($fh);

my $ship = { x => 0, y => 0, waypointX => 10, waypointY => 1 };

foreach my $line (@lines) {
    my $action = substr($line, 0, 1);
    my $value = substr($line, 1);
    chomp($value);
    processInstruction($ship, $action, $value);
}

my $manhattanDistance = abs($ship->{x}) + abs($ship->{y});
print "$manhattanDistance\n";

sub processInstruction {
    my ($ship, $action, $value) = @_;

    if ($action eq 'N') {
        $ship->{waypointY} += $value;
    } elsif ($action eq 'S') {
        $ship->{waypointY} -= $value;
    } elsif ($action eq 'E') {
        $ship->{waypointX} += $value;
    } elsif ($action eq 'W') {
        $ship->{waypointX} -= $value;
    } elsif ($action eq 'L') {
        rotateWaypoint($ship, -$value);
    } elsif ($action eq 'R') {
        rotateWaypoint($ship, $value);
    } elsif ($action eq 'F') {
        $ship->{x} += $ship->{waypointX} * $value;
        $ship->{y} += $ship->{waypointY} * $value;
    }
}

sub rotateWaypoint {
    my ($ship, $degrees) = @_;
    $degrees = ($degrees + 360) % 360;

    if ($degrees == 90 || $degrees == -270) {
        ($ship->{waypointX}, $ship->{waypointY}) = ($ship->{waypointY}, -$ship->{waypointX});
    } elsif ($degrees == 180 || $degrees == -180) {
        ($ship->{waypointX}, $ship->{waypointY}) = (-$ship->{waypointX}, -$ship->{waypointY});
    } elsif ($degrees == 270 || $degrees == -90) {
        ($ship->{waypointX}, $ship->{waypointY}) = (-$ship->{waypointY}, $ship->{waypointX});
    }
}

sub abs {
    my ($x) = @_;
    return $x < 0 ? -$x : $x;
}
