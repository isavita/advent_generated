use strict;
use warnings;

my %grid;
my ($startX, $startY);

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $y = 0;
while (my $line = <$fh>) {
    chomp $line;
    my @chars = split //, $line;
    for my $x (0 .. $#chars) {
        $grid{"$x,$y"} = 2 if $chars[$x] eq '#';
    }
    $startX //= int(@chars / 2);
    $startY = $y / 2;
    $y++;
}
close $fh;

my @dx = (0, 1, 0, -1);
my @dy = (-1, 0, 1, 0);

my ($x, $y, $dir) = ($startX, $startY, 0); # Start facing up
my $infectedCount = 0;

for (my $i = 0; $i < 10000000; $i++) {
    my $pos = "$x,$y";
    if (!exists $grid{$pos} || $grid{$pos} == 0) {
        $dir = ($dir - 1 + 4) % 4;
        $grid{$pos} = 1;
    } elsif ($grid{$pos} == 1) {
        $grid{$pos} = 2;
        $infectedCount++;
    } elsif ($grid{$pos} == 2) {
        $dir = ($dir + 1) % 4;
        $grid{$pos} = 3;
    } elsif ($grid{$pos} == 3) {
        $dir = ($dir + 2) % 4;
        $grid{$pos} = 0;
    }
    $x += $dx[$dir];
    $y += $dy[$dir];
}

print "$infectedCount\n";