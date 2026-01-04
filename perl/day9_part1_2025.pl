
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my @pts;
while (<$fh>) {
    push @pts, [split /,/];
}
close $fh;

my $best = 0;
for my $i (0..$#pts) {
    my ($x1, $y1) = @{$pts[$i]};
    for my $j ($i..$#pts) {
        my $area = (abs($x1 - $pts[$j][0]) + 1) * (abs($y1 - $pts[$j][1]) + 1);
        $best = $area if $area > $best;
    }
}
print "$best\n";
