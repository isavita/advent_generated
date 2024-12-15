
#!/usr/bin/perl
use strict;
use warnings;

my @dirs = ([1,0],[-1,0],[0,1],[0,-1]);
my @grid;
my @dp;
my $nr = 0;
my $nc = 0;

open my $fh, "<", "input.txt" or die "Could not open input.txt: $!";
while(<$fh>) {
    chomp;
    push @grid, [ map { int($_) } split // ];
    $nr++;
}
close $fh;
$nc = scalar @{$grid[0]};

for my $i (0..$nr-1) {
    push @dp, [ map { -1 } (0..$nc-1) ];
}

sub dfs {
    my ($r, $c) = @_;
    return $dp[$r][$c] if $dp[$r][$c] != -1;
    my $h = $grid[$r][$c];
    if ($h == 9) {
        $dp[$r][$c] = 1;
        return 1;
    }
    my $sum = 0;
    for my $d (@dirs) {
        my ($nr2, $nc2) = ($r + $d->[0], $c + $d->[1]);
        next if $nr2 < 0 || $nr2 >= $nr || $nc2 < 0 || $nc2 >= $nc;
        if ($grid[$nr2][$nc2] == $h + 1) {
            $sum += dfs($nr2, $nc2);
        }
    }
    $dp[$r][$c] = $sum;
    return $sum;
}

my $total = 0;
for my $r (0..$nr-1) {
    for my $c (0..$nc-1) {
        if ($grid[$r][$c] == 0) {
            $total += dfs($r, $c);
        }
    }
}
print $total, "\n";
