#!/usr/bin/perl

use strict;
use warnings;

my @directions = (
    [-1, -1], [0, -1], [1, -1],
    [-1,  0],          [1,  0],
    [-1,  1], [0,  1], [1,  1],
);

my @seating_area;
open my $file, '<', 'input.txt' or die "Error opening file: $!";
while (my $line = <$file>) {
    chomp $line;
    push @seating_area, [split //, $line];
}
close $file;

my $stabilized = 0;
while (!$stabilized) {
    my ($new_seating_area, $is_stabilized) = simulate_seating_part_two(\@seating_area);
    @seating_area = @$new_seating_area;
    $stabilized = $is_stabilized;
}

my $occupied_seats = count_occupied_seats(\@seating_area);
print "$occupied_seats\n";

sub simulate_seating_part_two {
    my ($seating_area) = @_;
    my $rows = scalar @$seating_area;
    my $cols = scalar @{$seating_area->[0]};
    my @new_seating_area = map { [@$_] } @$seating_area;
    my $stabilized = 1;

    for my $i (0 .. $rows - 1) {
        for my $j (0 .. $cols - 1) {
            if ($seating_area->[$i][$j] eq 'L') {
                if (count_visible_occupied($seating_area, $i, $j) == 0) {
                    $new_seating_area[$i][$j] = '#';
                    $stabilized = 0;
                }
            } elsif ($seating_area->[$i][$j] eq '#') {
                if (count_visible_occupied($seating_area, $i, $j) >= 5) {
                    $new_seating_area[$i][$j] = 'L';
                    $stabilized = 0;
                }
            }
        }
    }

    return (\@new_seating_area, $stabilized);
}

sub count_visible_occupied {
    my ($seating_area, $row, $col) = @_;
    my $count = 0;

    for my $dir (@directions) {
        my ($r, $c) = ($row + $dir->[0], $col + $dir->[1]);
        while ($r >= 0 && $r < @$seating_area && $c >= 0 && $c < @{$seating_area->[0]}) {
            if ($seating_area->[$r][$c] eq 'L') {
                last;
            } elsif ($seating_area->[$r][$c] eq '#') {
                $count++;
                last;
            }
            $r += $dir->[0];
            $c += $dir->[1];
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