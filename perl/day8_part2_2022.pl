use strict;
use warnings;

my %grid;
my $y = 0;
open my $file, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
while (my $line = <$file>) {
    chomp $line;
    my @chars = split //, $line;
    for my $x (0 .. $#chars) {
        $grid{"$x,$y"} = $chars[$x];
    }
    $y++;
}
close $file;

my @neighbors = ([0, 1], [0, -1], [1, 0], [-1, 0]);

my $max_score = 0;
for my $p (keys %grid) {
    my $score = 1;
    for my $n (@neighbors) {
        my ($next, $view) = ($p, 0);
        while (1) {
            my ($x, $y) = split /,/, $next;
            $x += $n->[0];
            $y += $n->[1];
            $next = "$x,$y";
            if (exists $grid{$next}) {
                $view++;
                if ($grid{$next} >= $grid{$p}) {
                    $score *= $view;
                    last;
                }
            } else {
                $score *= $view;
                last;
            }
        }
    }
    $max_score = $score if $score > $max_score;
}

print "$max_score\n";