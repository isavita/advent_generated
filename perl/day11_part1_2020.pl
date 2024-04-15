#!/usr/bin/perl

use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt': $!";
my @seating_area = map { chomp; [ split // ] } <$fh>;
close $fh;

my $stabilized = 0;
while (!$stabilized) {
    my ($new_seating_area, $is_stabilized) = simulate_seating(\@seating_area);
    @seating_area = @$new_seating_area;
    $stabilized = $is_stabilized;
}

my $occupied_seats = count_occupied_seats(\@seating_area);
print "$occupied_seats\n";

sub simulate_seating {
    my ($seating_area) = @_;
    my $rows = @$seating_area;
    my $cols = @{$seating_area->[0]};
    my @new_seating_area = map { [ @{$seating_area->[$_]} ] } 0..$rows-1;
    my $stabilized = 1;

    for my $i (0..$rows-1) {
        for my $j (0..$cols-1) {
            if ($seating_area->[$i][$j] eq 'L' && count_adjacent_occupied($seating_area, $i, $j) == 0) {
                $new_seating_area[$i][$j] = '#';
                $stabilized = 0;
            } elsif ($seating_area->[$i][$j] eq '#' && count_adjacent_occupied($seating_area, $i, $j) >= 4) {
                $new_seating_area[$i][$j] = 'L';
                $stabilized = 0;
            }
        }
    }

    return (\@new_seating_area, $stabilized);
}

sub count_adjacent_occupied {
    my ($seating_area, $row, $col) = @_;
    my $count = 0;
    for my $i ($row-1..$row+1) {
        for my $j ($col-1..$col+1) {
            next if $i == $row && $j == $col;
            if ($i >= 0 && $i < @$seating_area && $j >= 0 && $j < @{$seating_area->[0]} && $seating_area->[$i][$j] eq '#') {
                $count++;
            }
        }
    }
    return $count;
}

sub count_occupied_seats {
    my ($seating_area) = @_;
    my $count = 0;
    for my $row (@$seating_area) {
        for my $seat (@$row) {
            $count++ if $seat eq '#';
        }
    }
    return $count;
}