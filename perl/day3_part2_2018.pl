
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @claims;
while (my $line = <$fh>) {
    chomp $line;
    my @parts = split(' ', $line);
    my $id = substr($parts[0], 1);
    my @coords = split(',', substr($parts[2], 0, -1));
    my $x = $coords[0];
    my $y = $coords[1];
    my @dims = split('x', $parts[3]);
    my $width = $dims[0];
    my $height = $dims[1];
    push @claims, { ID => $id, X => $x, Y => $y, Width => $width, Height => $height };
}
close($fh);

my @fabric;
for (my $i = 0; $i < 1000; $i++) {
    $fabric[$i] = [(0) x 1000];
}

foreach my $claim (@claims) {
    for (my $y = $claim->{Y}; $y < $claim->{Y} + $claim->{Height}; $y++) {
        for (my $x = $claim->{X}; $x < $claim->{X} + $claim->{Width}; $x++) {
            $fabric[$y][$x]++;
        }
    }
}

foreach my $claim (@claims) {
    my $overlap = 0;
    for (my $y = $claim->{Y}; $y < $claim->{Y} + $claim->{Height}; $y++) {
        for (my $x = $claim->{X}; $x < $claim->{X} + $claim->{Width}; $x++) {
            if ($fabric[$y][$x] > 1) {
                $overlap = 1;
                last;
            }
        }
        if ($overlap) {
            last;
        }
    }
    if (!$overlap) {
        print $claim->{ID} . "\n";
        exit;
    }
}
