
use strict;
use warnings;

my %activeCubes;
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
while (my $line = <$fh>) {
    chomp $line;
    my @chars = split('', $line);
    for my $x (0..$#chars) {
        $activeCubes{"$x,$.,0,0"} = 1 if $chars[$x] eq '#';
    }
}
close($fh);

for my $cycle (0..5) {
    %activeCubes = simulateCycle4D(%activeCubes);
}

print scalar keys %activeCubes;

sub simulateCycle4D {
    my %activeCubes = @_;
    my %newActiveCubes;
    my %neighborCounts;

    foreach my $coord (keys %activeCubes) {
        for my $dw (-1..1) {
            for my $dz (-1..1) {
                for my $dy (-1..1) {
                    for my $dx (-1..1) {
                        next if $dw == 0 && $dz == 0 && $dy == 0 && $dx == 0;
                        my ($x, $y, $z, $w) = split(',', $coord);
                        my $neighbor = join(',', $x + $dx, $y + $dy, $z + $dz, $w + $dw);
                        $neighborCounts{$neighbor}++;
                    }
                }
            }
        }
    }

    foreach my $coord (keys %neighborCounts) {
        my $count = $neighborCounts{$coord};
        $newActiveCubes{$coord} = 1 if $count == 3 || ($count == 2 && $activeCubes{$coord});
    }

    return %newActiveCubes;
}
