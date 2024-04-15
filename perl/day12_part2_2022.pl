use strict;
use warnings;
use List::Util qw(min);

my %grid;
my ($start, $end);
my @as;
my $y = 0;
open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
while (my $line = <$fh>) {
    chomp $line;
    my @chars = split //, $line;
    for my $x (0 .. $#chars) {
        my $p = "$x,$y";
        $grid{$p} = $chars[$x];
        if ($chars[$x] eq 'S') {
            $start = $p;
        } elsif ($chars[$x] eq 'E') {
            $end = $p;
        } elsif ($chars[$x] eq 'a') {
            push @as, $p;
        }
    }
    $y++;
}
close $fh;
$grid{$start} = 'a';
$grid{$end} = 'z';

my %dists = djikstra(\%grid, $end);

my $l = $dists{$start};

for my $a (@as) {
    if (exists $dists{$a}) {
        $l = min($l, $dists{$a});
    }
}
print "$l\n";

sub djikstra {
    my ($grid, $end) = @_;
    my %dist = ($end => 0);
    my @pq = ([$end, 0]);
    while (@pq) {
        my ($curr, $curr_dist) = @{shift @pq};
        for my $n ([0, 1], [0, -1], [1, 0], [-1, 0]) {
            my ($x, $y) = split /,/, $curr;
            my $next = ($x + $n->[0]) . ',' . ($y + $n->[1]);
            next unless exists $grid->{$next};
            my $curr_val = ord($grid->{$curr});
            my $next_val = ord($grid->{$next});
            next if $curr_val - $next_val > 1;
            my $next_dist = $dist{$curr} + 1;
            if (!exists $dist{$next} || $next_dist < $dist{$next}) {
                $dist{$next} = $next_dist;
                push @pq, [$next, $next_dist];
                @pq = sort { $a->[1] <=> $b->[1] } @pq;
            }
        }
    }
    return %dist;
}