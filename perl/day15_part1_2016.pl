
use strict;
use warnings;

my @discs;
open(my $fh, '<', 'input.txt') or die $!;
while (my $line = <$fh>) {
    chomp $line;
    if ($line =~ /Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)./){
        my $totalPositions = $2;
        my $startPosition = $3;
        push @discs, {totalPositions => $totalPositions, startPosition => $startPosition};
    }
}
close($fh);

my $time = 0;
while (1) {
    if (checkDiscs(\@discs, $time)) {
        print "$time\n";
        last;
    }
    $time++;
}

sub checkDiscs {
    my ($discs, $time) = @_;
    foreach my $i (0..$#{$discs}) {
        my $position = ($discs->[$i]{startPosition} + $time + $i + 1) % $discs->[$i]{totalPositions};
        if ($position != 0) {
            return 0;
        }
    }
    return 1;
}
