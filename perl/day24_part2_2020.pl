use strict;
use warnings;

my %directions = (
    "e"  => [1, 0],
    "se" => [0, 1],
    "sw" => [-1, 1],
    "w"  => [-1, 0],
    "nw" => [0, -1],
    "ne" => [1, -1]
);

sub getNeighbors {
    my ($tile) = @_;
    my @neighbors;
    foreach my $dir (values %directions) {
        push @neighbors, [$tile->[0] + $dir->[0], $tile->[1] + $dir->[1]];
    }
    return @neighbors;
}

open my $fh, '<', 'input.txt' or die $!;
my %blackTiles;

while (my $line = <$fh>) {
    chomp $line;
    my @coord = (0, 0);
    for (my $i = 0; $i < length($line); $i++) {
        my $dir;
        my $char = substr($line, $i, 1);
        if ($char eq 'e' || $char eq 'w') {
            $dir = $char;
        } elsif ($char eq 'n' || $char eq 's') {
            $dir = substr($line, $i, 2);
            $i++;
        }
        my $move = $directions{$dir};
        $coord[0] += $move->[0];
        $coord[1] += $move->[1];
    }
    $blackTiles{"@coord"} = !$blackTiles{"@coord"};
}

for (my $day = 0; $day < 100; $day++) {
    my %tilesToCheck;
    foreach my $tile (keys %blackTiles) {
        if ($blackTiles{$tile}) {
            $tilesToCheck{$tile} = 1;
            foreach my $neighbor (getNeighbors([split ' ', $tile])) {
                $tilesToCheck{"@{$neighbor}"} = 1;
            }
        }
    }

    my %newBlackTiles;
    foreach my $tile (keys %tilesToCheck) {
        my $blackNeighborCount = 0;
        my @currentTile = split ' ', $tile;
        foreach my $neighbor (getNeighbors(\@currentTile)) {
            $blackNeighborCount++ if $blackTiles{"@{$neighbor}"};
        }
        if ($blackTiles{$tile} && ($blackNeighborCount == 1 || $blackNeighborCount == 2)) {
            $newBlackTiles{$tile} = 1;
        } elsif (!$blackTiles{$tile} && $blackNeighborCount == 2) {
            $newBlackTiles{$tile} = 1;
        }
    }

    %blackTiles = %newBlackTiles;
}

print scalar(keys %blackTiles), "\n";