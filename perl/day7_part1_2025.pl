#!/usr/bin/perl
use strict;
use warnings;
use autodie;

my @grid = grep { $_ ne '' } map { chomp; $_ } do {
    open my $fh, '<', 'input.txt';
    <$fh>;
};

exit if !@grid;
my $height = @grid;
my $width  = length $grid[0];

my ($sx,$sy);
for my $y (0..$height-1) {
    if ( (my $pos = index($grid[$y],'S')) != -1 ) {
        ($sx,$sy) = ($pos,$y);
        last;
    }
}
die "Start not found" unless defined $sx;

my %active = ( $sx => 1 );
my $splits = 0;

for my $y ($sy..$height-1) {
    my %next;
    for my $x (keys %active) {
        next if $x < 0 || $x >= $width;
        my $cell = substr $grid[$y], $x, 1;
        if ($cell eq '^') {
            ++$splits;
            $next{$x-1}=1 if $x-1>=0;
            $next{$x+1}=1 if $x+1<$width;
        } else {
            $next{$x}=1;
        }
    }
    %active = %next;
    last unless %active;
}

print "Total times the beam is split: $splits\n";