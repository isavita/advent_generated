use strict;
use warnings;
use List::Util qw(min);

my %grid;
my @as;
my ($start, $end);

open my $fh, "<", "input.txt" or die $!;
my $y = 0;
while (my $line = <$fh>) {
    chomp $line;
    my @chars = split("", $line);
    for my $x (0..$#chars) {
        my $b = $chars[$x];
        my $p = "$x,$y";
        $grid{$p} = $b;
        $start = $p if $b eq 'S';
        $end = $p if $b eq 'E';
        push @as, $p if $b eq 'a';
    }
    $y++;
}
close $fh;

$grid{$start} = 'a';
$grid{$end} = 'z';

my $dists = djikstra(\%grid, $end);

my $l = $dists->{$start};
print "$l\n";

sub djikstra {
    my ($grid, $end) = @_;
    my @pq;
    my %dist = ($end => 0);
    push @pq, {obj => $end, priority => 0};
    
    while (@pq) {
        my $curr = pop @pq;
        my $curr_obj = $curr->{obj};
        
        for my $n ([0, 1], [0, -1], [1, 0], [-1, 0]) {
            my ($dx, $dy) = @$n;
            my ($x, $y) = split(",", $curr_obj);
            my $next = ($x + $dx) . "," . ($y + $dy);
            
            next unless exists $grid->{$next};
            next if (ord($grid->{$curr_obj}) - ord($grid->{$next})) > 1;
            
            my $nextdist = $dist{$curr_obj} + 1;
            if (!exists $dist{$next} || $nextdist < $dist{$next}) {
                $dist{$next} = $nextdist;
                unshift @pq, {obj => $next, priority => $nextdist};
            }
        }
    }
    
    return \%dist;
}