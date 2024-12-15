
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

my @grid;
open my $fh, "<", "input.txt" or die "Could not open input.txt: $!";
while (<$fh>) {
    chomp;
    push @grid, [ map { int($_) } split // ];
}
close $fh;

my $nr = @grid;
my $nc = @{$grid[0]};
my @dirs = ([1,0],[-1,0],[0,1],[0,-1]);
my @trailheads;

for my $r (0..$nr-1) {
    for my $c (0..$nc-1) {
        if ($grid[$r][$c] == 0) {
            push @trailheads, [$r, $c];
        }
    }
}

my $sumScores = 0;
for my $th (@trailheads) {
    my %reached;
    my @front = ([@$th, 0]);
    my %visited;
    while (@front) {
        my ($r, $c, $h) = @{pop @front};
        if ($h == 9) {
            $reached{"$r,$c"} = 1 unless $reached{"$r,$c"};
            next;
        }
        for my $d (@dirs) {
            my ($nr2, $nc2) = ($r + $d->[0], $c + $d->[1]);
            next if $nr2 < 0 || $nr2 >= $nr || $nc2 < 0 || $nc2 >= $nc;
            if ($grid[$nr2][$nc2] == $h + 1) {
                my $key = "$nr2,$nc2,".($h+1);
                unless ($visited{$key}) {
                    $visited{$key} = 1;
                    push @front, [$nr2, $nc2, $h + 1];
                }
            }
        }
    }
    $sumScores += scalar keys %reached;
}

print $sumScores, "\n";
