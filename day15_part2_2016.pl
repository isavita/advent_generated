
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my @discs;
my $discRegex = qr/Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)./;

while (my $line = <$fh>) {
    chomp $line;
    if (my @matches = $line =~ /$discRegex/) {
        my ($totalPositions, $startPosition) = ($matches[1], $matches[2]);
        push @discs, { totalPositions => $totalPositions, startPosition => $startPosition };
    }
}

push @discs, { totalPositions => 11, startPosition => 0 };

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
    for my $i (0..$#{$discs}) {
        my $disc = $discs->[$i];
        my $position = ($disc->{startPosition} + $time + $i + 1) % $disc->{totalPositions};
        if ($position != 0) {
            return 0;
        }
    }
    return 1;
}
