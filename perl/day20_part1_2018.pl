#!/usr/bin/perl

use strict;
use warnings;

sub build_map {
    my $regex = shift;
    my %dm;
    my @stack;
    my $cp = [0, 0];
    
    foreach my $c (split //, $regex) {
        if ($c eq '(') {
            push @stack, [@$cp];
        } elsif ($c eq '|') {
            $cp = $stack[-1];
        } elsif ($c eq ')') {
            $cp = pop @stack;
        } else {
            my $np = move($cp, $c);
            $dm{"@$cp"}{"@$np"} = 1;
            $cp = $np;
        }
    }
    
    return \%dm;
}

sub move {
    my ($p, $dir) = @_;
    my ($x, $y) = @$p;
    
    if ($dir eq 'N') {
        return [$x, $y - 1];
    } elsif ($dir eq 'S') {
        return [$x, $y + 1];
    } elsif ($dir eq 'E') {
        return [$x + 1, $y];
    } elsif ($dir eq 'W') {
        return [$x - 1, $y];
    }
    
    return $p;
}

sub find_furthest_room {
    my $dm = shift;
    my %visited;
    my @queue = ([0, 0]);
    my $max_doors = 0;
    
    while (@queue) {
        my $p = shift @queue;
        foreach my $np (keys %{$dm->{"@$p"}}) {
            if (!exists $visited{$np}) {
                $visited{$np} = $visited{"@$p"} + 1;
                $max_doors = max($max_doors, $visited{$np});
                push @queue, [split ' ', $np];
            }
        }
    }
    
    return $max_doors;
}

sub max {
    my ($a, $b) = @_;
    return $a > $b ? $a : $b;
}

open my $fh, '<', 'input.txt' or die $!;
my $regex = <$fh>;
close $fh;

chomp $regex;
$regex =~ s/^(\(|\|)|\)$//g;

my $dm = build_map($regex);
my $max_doors = find_furthest_room($dm);
print "$max_doors\n";