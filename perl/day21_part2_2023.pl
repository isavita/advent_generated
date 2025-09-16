#!/usr/bin/perl
use strict;
use warnings;
use Math::BigInt;

sub count_reachable {
    my ($grid_ref, $height, $width, $start_pos, $target_steps, $infinite) = @_;
    my %current_positions;
    $current_positions{ $start_pos->{y} . "," . $start_pos->{x} } = 1;
    my %counts_at_steps;
    my @deltas = ( [-1,0], [1,0], [0,-1], [0,1] );

    for (my $step = 1; $step <= $target_steps; $step++) {
        my %next_positions;
        foreach my $pos_key (keys %current_positions) {
            my ($cy, $cx) = split /,/, $pos_key;
            foreach my $d (@deltas) {
                my ($dy, $dx) = @{$d};
                my $ny = $cy + $dy;
                my $nx = $cx + $dx;
                my $nkey = $ny . "," . $nx;
                my $valid = 0;
                if ($infinite) {
                    my $map_y = (($ny - 1) % $height);
                    $map_y += $height if $map_y < 0;
                    $map_y += 1;
                    my $map_x = (($nx - 1) % $width);
                    $map_x += $width if $map_x < 0;
                    $map_x += 1;
                    my $char = $grid_ref->[$map_y]->[$map_x];
                    if (defined $char && $char ne '#') { $valid = 1; }
                } else {
                    if ($ny >= 1 && $ny <= $height && $nx >= 1 && $nx <= $width) {
                        my $char = $grid_ref->[$ny]->[$nx];
                        if (defined $char && $char ne '#') { $valid = 1; }
                    }
                }
                if ($valid) { $next_positions{$nkey} = 1; }
            }
        }
        %current_positions = %next_positions;
        if ($infinite) {
            my $offset = int($width / 2);
            if ($step == $offset || $step == $offset + $width || $step == $offset + 2*$width) {
                my $count = scalar keys %current_positions;
                $counts_at_steps{$step} = $count;
            }
        }
    }

    my $final_count = scalar keys %current_positions;
    if ($infinite) {
        return ($final_count, \%counts_at_steps);
    } else {
        return ($final_count);
    }
}

open my $fh, "<", "input.txt" or die "Could not open input.txt";
my @lines = <$fh>;
close $fh;
chomp @lines;

my $height = scalar @lines;
my $width = length $lines[0];
my @grid;
my ($start_y, $start_x);

for (my $y = 1; $y <= $height; $y++) {
    my $line = $lines[$y-1];
    for (my $x = 1; $x <= $width; $x++) {
        my $ch = substr($line, $x-1, 1);
        $grid[$y][$x] = $ch;
        if ($ch eq 'S') {
            die "Multiple start positions 'S' found" if defined $start_y;
            $start_y = $y;
            $start_x = $x;
            $grid[$y][$x] = '.';
        }
    }
}
die "Start position 'S' not found" unless defined $start_y;
my $start_pos = { y => $start_y, x => $start_x };

my $steps1 = 64;
my ($part1) = count_reachable(\@grid, $height, $width, $start_pos, $steps1, 0);
print "Part 1: $part1\n";

my $steps2 = 26501365;
my $size = $width;
my $offset = int($size / 2);
my $s0 = $offset;
my $s1 = $offset + $size;
my $s2 = $offset + 2 * $size;

my ($final2, $counts_ref) = count_reachable(\@grid, $height, $width, $start_pos, $s2, 1);
my $y0 = $counts_ref->{$s0};
my $y1 = $counts_ref->{$s1};
my $y2 = $counts_ref->{$s2};
die "Missing counts for quadratic fit" unless defined $y0 && defined $y1 && defined $y2;

my $c = $y0;
my $a_num = $y2 - 2 * $y1 + $y0;
my $a_int = int($a_num / 2);
my $a_rem = $a_num % 2;
my $a = $a_int;
my $b = ($y1 - $y0) - $a;

my $k = int(($steps2 - $offset) / $size);

my $A = Math::BigInt->new($a);
my $B = Math::BigInt->new($b);
my $C = Math::BigInt->new($c);
my $K = Math::BigInt->new($k);

my $K2 = $K * $K;
my $part2 = $A * $K2 + $B * $K + $C;

print "Part 2: " . $part2 . "\n";