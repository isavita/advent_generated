use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my @stars;
while (<$fh>) {
    if (/position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/) {
        push @stars, [$1, $2, $3, $4];
    }
}
close $fh;

my $smallest_t = 0;
my $smallest_area = 2**31 - 1;
my $time_limit = 100000;

for my $t (1..$time_limit) {
    my ($max_x, $max_y, $min_x, $min_y) = (0, 0, 2**31-1, 2**31-1);
    
    foreach my $star (@stars) {
        my $x = $star->[0] + $star->[2] * $t;
        my $y = $star->[1] + $star->[3] * $t;
        $max_x = $x if $x > $max_x;
        $min_x = $x if $x < $min_x;
        $max_y = $y if $y > $max_y;
        $min_y = $y if $y < $min_y;
    }
    
    my $width = $max_x - $min_x + 1;
    my $height = $max_y - $min_y + 1;
    my $area = $width * $height;
    
    if ($smallest_area > $area) {
        $smallest_area = $area;
        $smallest_t = $t;
    }
}

my $t = $smallest_t;
my ($max_x, $max_y, $min_x, $min_y) = (0, 0, 2**31-1, 2**31-1);

foreach my $star (@stars) {
    $star->[0] += $star->[2] * $t;
    $star->[1] += $star->[3] * $t;
    $max_x = $star->[0] if $star->[0] > $max_x;
    $min_x = $star->[0] if $star->[0] < $min_x;
    $max_y = $star->[1] if $star->[1] > $max_y;
    $min_y = $star->[1] if $star->[1] < $min_y;
}

my @grid = map { [(' ') x ($max_x - $min_x + 1)] } 0..($max_y - $min_y);

foreach my $star (@stars) {
    $grid[$star->[1] - $min_y][$star->[0] - $min_x] = '#';
}

foreach my $line (@grid) {
    print join('', @$line), "\n";
}