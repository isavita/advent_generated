
use strict;
use warnings;

my %directions = (
    "e"  => { q => 1, r => 0 },
    "se" => { q => 0, r => 1 },
    "sw" => { q => -1, r => 1 },
    "w"  => { q => -1, r => 0 },
    "nw" => { q => 0, r => -1 },
    "ne" => { q => 1, r => -1 }
);

my %blackTiles;
open(my $fh, "<", "input.txt") or die "Cannot open file: $!";
while (my $line = <$fh>) {
    chomp($line);
    my @chars = split("", $line);
    my $coord = { q => 0, r => 0 };

    for (my $i = 0; $i < scalar(@chars); $i++) {
        my $dir;
        if ($chars[$i] eq 'e' || $chars[$i] eq 'w') {
            $dir = $chars[$i];
        } else {
            $dir = $chars[$i] . $chars[$i + 1];
            $i++;
        }
        my $move = $directions{$dir};
        $coord->{q} += $move->{q};
        $coord->{r} += $move->{r};
    }

    $blackTiles{join(",", $coord->{q}, $coord->{r})} = !$blackTiles{join(",", $coord->{q}, $coord->{r})};
}
close($fh);

my $count = 0;
foreach my $black (values %blackTiles) {
    if ($black) {
        $count++;
    }
}
print "$count\n";
