
use strict;
use warnings;

sub manhattan {
    my ($p_x, $p_y, $q_x, $q_y) = @_;
    return abs($p_x - $q_x) + abs($p_y - $q_y);
}

my @sensors;
open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
while (my $line = <$fh>) {
    my ($s_x, $s_y, $b_x, $b_y);
    ($s_x, $s_y, $b_x, $b_y) = ($line =~ /Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/);
    my $dist = manhattan($s_x, $s_y, $b_x, $b_y);
    push @sensors, { pos_x => $s_x, pos_y => $s_y, beacon_x => $b_x, beacon_y => $b_y, dist => $dist };
}
close $fh;

my $maxcoord = 4000000;
for (my $x = 0; $x <= $maxcoord; $x++) {
    my $y = 0;
    while ($y <= $maxcoord) {
        my $detected = 0;
        my $skip = 0;
        for my $s (@sensors) {
            if (manhattan($s->{pos_x}, $s->{pos_y}, $x, $y) <= $s->{dist}) {
                $detected = 1;
                my $dist = $s->{dist} - abs($s->{pos_x} - $x);
                $skip = $dist + $s->{pos_y} - $y if ($dist + $s->{pos_y} - $y > $skip);
            }
        }
        if (!$detected) {
            print $x * 4000000 + $y;
            exit;
        }
        $y += $skip > 0 ? $skip : 1;
    }
}
