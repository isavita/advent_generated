
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(reduce);

my @neighbors = (
    [0, 1], [0, -1], [1, 0], [-1, 0],
    [-1, -1], [-1, 1], [1, -1], [1, 1]
);

my %grid;
my @parts;
my %parts_grid;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my @lines = <$fh>;
close $fh;

my $curr = undef;
for my $y (0..$#lines) {
    chomp $lines[$y];
    my $line = $lines[$y];
    
    for my $x (0..length($line)-1) {
        my $c = substr($line, $x, 1);
        $grid{"$x,$y"} = $c;
        
        if ($c =~ /\d/) {
            if (!defined $curr) {
                $curr = {
                    xmin => $x,
                    xmax => $x,
                    y => $y,
                    n => int($c)
                };
            } else {
                $curr->{n} = $curr->{n} * 10 + int($c);
                $curr->{xmax} = $x;
            }
        } elsif (defined $curr) {
            push @parts, $curr;
            $curr = undef;
        }
    }
    
    push @parts, $curr if defined $curr;
    $curr = undef;
}

for my $i (0..$#parts) {
    my $p = $parts[$i];
    for my $x ($p->{xmin}..$p->{xmax}) {
        $parts_grid{"$x,$p->{y}"} = $i;
    }
}

my $sum = 0;
for my $p (keys %grid) {
    my $c = $grid{$p};
    next unless $c eq '*';
    
    my %neighbor_parts;
    for my $n (@neighbors) {
        my ($nx, $ny) = split(',', $p);
        $nx += $n->[0];
        $ny += $n->[1];
        
        if (exists $parts_grid{"$nx,$ny"}) {
            $neighbor_parts{$parts_grid{"$nx,$ny"}} = 1;
        }
    }
    
    if (keys %neighbor_parts == 2) {
        my $prod = reduce { $a * $b } map { $parts[$_]->{n} } keys %neighbor_parts;
        $sum += $prod;
    }
}

print "$sum\n";
