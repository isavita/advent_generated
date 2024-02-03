
use strict;
use warnings;

my @nanobots = parseNanobots("input.txt");

my $strongest = findStrongestNanobot(\@nanobots);
my $inRangeCount = countNanobotsInRange(\@nanobots, $strongest);

print "$inRangeCount\n";

sub parseNanobots {
    my ($file) = @_;
    open my $fh, '<', $file or die $!;
    my @nanobots;
    my $re = qr/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/;

    while (my $line = <$fh>) {
        chomp $line;
        my @matches = $line =~ /$re/;

        my $x = $matches[0];
        my $y = $matches[1];
        my $z = $matches[2];
        my $radius = $matches[3];

        push @nanobots, { X => $x, Y => $y, Z => $z, Radius => $radius };
    }

    close $fh;
    return @nanobots;
}

sub findStrongestNanobot {
    my ($nanobots) = @_;
    my $strongest;
    for my $nanobot (@$nanobots) {
        if (!$strongest || $nanobot->{Radius} > $strongest->{Radius}) {
            $strongest = $nanobot;
        }
    }
    return $strongest;
}

sub countNanobotsInRange {
    my ($nanobots, $strongest) = @_;
    my $count = 0;
    for my $nanobot (@$nanobots) {
        if (manhattanDistance($nanobot, $strongest) <= $strongest->{Radius}) {
            $count++;
        }
    }
    return $count;
}

sub manhattanDistance {
    my ($a, $b) = @_;
    return abs($a->{X} - $b->{X}) + abs($a->{Y} - $b->{Y}) + abs($a->{Z} - $b->{Z});
}

sub abs {
    my ($x) = @_;
    return $x < 0 ? -$x : $x;
}
