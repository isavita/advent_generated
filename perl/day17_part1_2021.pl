
use strict;
use warnings;

# Read the target area from input.txt
open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my $line = <$fh>;
close $fh;

# Parse the target area
my ($x_min, $x_max, $y_min, $y_max) = $line =~ /x=(\d+)\.\.(\d+), y=(-?\d+)\.\.(-?\d+)/;

my $max_height = 0;

# Iterate through possible initial velocities
for my $vx (0 .. $x_max) {
    for my $vy ($y_min .. abs($y_min)) {
        my ($x, $y) = (0, 0);
        my ($current_vx, $current_vy) = ($vx, $vy);
        my $current_max_height = 0;

        # Simulate the probe's trajectory
        while ($x <= $x_max && $y >= $y_min) {
            $x += $current_vx;
            $y += $current_vy;
            $current_max_height = $y if $y > $current_max_height;

            # Check if within target area
            if ($x >= $x_min && $x <= $x_max && $y >= $y_min && $y <= $y_max) {
                $max_height = $current_max_height if $current_max_height > $max_height;
                last;  # Found a valid trajectory, no need to continue
            }

            # Update velocities
            $current_vx-- if $current_vx > 0;
            $current_vy--;
        }
    }
}

print "The highest y position reached is: $max_height\n";
