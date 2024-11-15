
#!/usr/bin/perl
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Cannot open input file: $!";
my $serial = <$fh>;
chomp $serial;
close $fh;

use constant GRID_SIZE => 300;

my @grid;
my @prefix_sum;

# Calculate power levels and prefix sum
for my $y (0..GRID_SIZE-1) {
    for my $x (0..GRID_SIZE-1) {
        my $rack_id = $x + 11;
        my $power_level = $rack_id * ($y + 1);
        $power_level += $serial;
        $power_level *= $rack_id;
        $power_level = int(($power_level / 100) % 10) - 5;
        $grid[$y][$x] = $power_level;
        
        # Compute 2D prefix sum
        $prefix_sum[$y][$x] = $power_level 
            + ($y > 0 ? $prefix_sum[$y-1][$x] : 0)
            + ($x > 0 ? $prefix_sum[$y][$x-1] : 0)
            - ($y > 0 && $x > 0 ? $prefix_sum[$y-1][$x-1] : 0);
    }
}

my ($max_x, $max_y, $max_size, $max_power) = (0, 0, 0, -999999);

# Find max power square using prefix sum
for my $size (1..GRID_SIZE) {
    for my $y (0..GRID_SIZE-$size) {
        for my $x (0..GRID_SIZE-$size) {
            my $total_power = $prefix_sum[$y+$size-1][$x+$size-1]
                - ($y > 0 ? $prefix_sum[$y-1][$x+$size-1] : 0)
                - ($x > 0 ? $prefix_sum[$y+$size-1][$x-1] : 0)
                + ($y > 0 && $x > 0 ? $prefix_sum[$y-1][$x-1] : 0);
            
            if ($total_power > $max_power) {
                $max_power = $total_power;
                $max_x = $x + 1;
                $max_y = $y + 1;
                $max_size = $size;
            }
        }
    }
}

print "$max_x,$max_y,$max_size\n";
