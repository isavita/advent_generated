
use strict;
use warnings;

my %grid;
my ($startX, $startY);

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $y = 0;
while (my $line = <$fh>) {
    chomp $line;
    my @chars = split('', $line);
    for my $x (0..$#chars) {
        if ($chars[$x] eq '#') {
            $grid{"$x,$y"} = 1;
        }
    }
    $startX = int(@chars/2);
    $startY = $y/2;
    $y++;
}
close($fh);

my @dx = (0, 1, 0, -1);
my @dy = (-1, 0, 1, 0);

my ($x, $y, $dir) = ($startX, $startY, 0);
my $infectedCount = 0;

for (my $i = 0; $i < 10000; $i++) {
    my $pos = "$x,$y";
    if ($grid{$pos}) {
        $dir = ($dir + 1) % 4;
        delete $grid{$pos};
    } else {
        $dir = ($dir - 1 + 4) % 4;
        $grid{$pos} = 1;
        $infectedCount++;
    }
    $x += $dx[$dir];
    $y += $dy[$dir];
}

print "$infectedCount\n";
