
use strict;
use warnings;
use bigint;

my @grid;
open my $fh, '<', 'input.txt' or die $!;
while (<$fh>) {
    chomp;
    push @grid, $_ if $_ ne '';
}
close $fh;

if (!@grid) {
    print "0\n";
    exit;
}

my $height = @grid;
my $width  = length $grid[0];

my ($start_x, $start_y);
OUTER: for my $y (0 .. $height - 1) {
    for my $x (0 .. $width - 1) {
        if (substr($grid[$y], $x, 1) eq 'S') {
            ($start_x, $start_y) = ($x, $y);
            last OUTER;
        }
    }
}
die "Start point 'S' not found" unless defined $start_x;

my %counts = ($start_x => 1);

for my $y ($start_y .. $height - 1) {
    my %next;
    for my $x (keys %counts) {
        my $cnt = $counts{$x};
        my $split = 0;
        if ($x >= 0 && $x < $width) {
            $split = 1 if substr($grid[$y], $x, 1) eq '^';
        }
        if ($split) {
            $next{$x - 1} = ($next{$x - 1} // 0) + $cnt;
            $next{$x + 1} = ($next{$x + 1} // 0) + $cnt;
        } else {
            $next{$x} = ($next{$x} // 0) + $cnt;
        }
    }
    %counts = %next;
}

my $total = 0;
$total += $_ for values %counts;
print "Total different timelines: $total\n";
