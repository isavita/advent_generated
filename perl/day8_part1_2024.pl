
#!/usr/bin/perl
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die "Can't open input.txt: $!";
my @grid = <$fh>;
close $fh;
chomp @grid;

my $h = @grid;
my $w = length $grid[0];
my %antennas;

for my $y (0..$h-1) {
    for my $x (0..$w-1) {
        my $c = substr($grid[$y], $x, 1);
        next if $c eq '.';
        push @{$antennas{$c}}, [$y, $x];
    }
}

my %antinodes;
for my $coords (values %antennas) {
    my $n = @$coords;
    for my $i (0..$n-1) {
        for my $j ($i+1..$n-1) {
            my ($Ay, $Ax) = @{$coords->[$i]};
            my ($By, $Bx) = @{$coords->[$j]};
            my $P1y = 2*$Ay - $By;
            my $P1x = 2*$Ax - $Bx;
            my $P2y = 2*$By - $Ay;
            my $P2x = 2*$Bx - $Ax;
            $antinodes{"$P1y,$P1x"} = 1 if $P1y >= 0 && $P1y < $h && $P1x >= 0 && $P1x < $w;
            $antinodes{"$P2y,$P2x"} = 1 if $P2y >= 0 && $P2y < $h && $P2x >= 0 && $P2x < $w;
        }
    }
}

print scalar keys %antinodes, "\n";
