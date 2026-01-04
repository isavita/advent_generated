
use strict;
use warnings;
my @grid;
open my $fh, '<', 'input.txt' or die;
while (my $line = <$fh>) {
    chomp $line;
    push @grid, $line;
}
close $fh;
my $R = scalar @grid;
my $C = 0;
$C = $_ > $C ? $_ : $C for map { length } @grid;
if (!$R || !$C) {
    print "Total rolls removed: 0\n";
    exit;
}
# pad rows
$_ .= '.' x ($C - length $_) for @grid;

my @dr = (-1,-1,-1, 0,0, 1,1,1);
my @dc = (-1, 0, 1,-1,1,-1,0,1);
my $removed = 0;

while (1) {
    my $changed = 0;
    for my $r (0 .. $R-1) {
        for my $c (0 .. $C-1) {
            next unless substr($grid[$r], $c, 1) eq '@';
            my $cnt = 0;
            for my $k (0 .. 7) {
                my $nr = $r + $dr[$k];
                my $nc = $c + $dc[$k];
                next if $nr < 0 || $nr >= $R || $nc < 0 || $nc >= $C;
                $cnt++ if substr($grid[$nr], $nc, 1) eq '@';
            }
            if ($cnt < 4) {
                substr($grid[$r], $c, 1) = '*';
                $changed = 1;
            }
        }
    }
    last unless $changed;
    for my $r (0 .. $R-1) {
        for my $c (0 .. $C-1) {
            if (substr($grid[$r], $c, 1) eq '*') {
                substr($grid[$r], $c, 1) = '.';
                $removed++;
            }
        }
    }
}
print "Total rolls removed: $removed\n";
