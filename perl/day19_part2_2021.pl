#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(max);
use POSIX;

# This program solves the Day 19 challenge of Advent of Code 2021.
# It reads the input from "input.txt", which contains scanner reports,
# finds the positions of all scanners and beacons (by matching 12 overlapping beacons)
# and then prints:
#    * the total number of unique beacons (Part One)
#    * the largest Manhattan distance between any two scanners (Part Two)

# --- Helper functions ---

# Manhattan distance between two points
sub manhattan {
    my ($p, $q) = @_;
    return abs($p->[0]-$q->[0]) + abs($p->[1]-$q->[1]) + abs($p->[2]-$q->[2]);
}

# Add two 3D vectors
sub add {
    my ($a, $b) = @_;
    return [$a->[0]+$b->[0], $a->[1]+$b->[1], $a->[2]+$b->[2]];
}

# Subtract two 3D vectors: a - b
sub sub_vec {
    my ($a, $b) = @_;
    return [$a->[0]-$b->[0], $a->[1]-$b->[1], $a->[2]-$b->[2]];
}

# Convert a coordinate triple to a string key: "x,y,z"
sub pt2key {
    my ($pt) = @_;
    return join(",", @$pt);
}

# --- Generate all 24 rotation functions ---
# Each rotation is represented as a sub (closure) that maps [x,y,z] -> [x',y',z']
# We'll generate them by choosing a candidate new x-axis from the 6 possibilities,
# then choose a new y-axis among the ones orthogonal to new x, and deduce new z = cross(new_x, new_y).
# We only keep rotation matrices with determinant 1 (a proper rotation) and ensure uniqueness.
sub generate_rotations {
    my @rots;
    my @axes = (
      [ 1,  0,  0],
      [-1,  0,  0],
      [ 0,  1,  0],
      [ 0, -1,  0],
      [ 0,  0,  1],
      [ 0,  0, -1],
    );
    # dot product
    sub dot {
        my ($a, $b) = @_;
        return $a->[0]*$b->[0] + $a->[1]*$b->[1] + $a->[2]*$b->[2];
    }
    # cross product
    sub cross {
        my ($a, $b) = @_;
        return [
            $a->[1]*$b->[2] - $a->[2]*$b->[1],
            $a->[2]*$b->[0] - $a->[0]*$b->[2],
            $a->[0]*$b->[1] - $a->[1]*$b->[0],
        ];
    }
    # determinant of 3x3 matrix (represented as columns)
    sub det {
        my ($x, $y, $z) = @_;
        return $x->[0]*($y->[1]*$z->[2] - $y->[2]*$z->[1])
             - $x->[1]*($y->[0]*$z->[2] - $y->[2]*$z->[0])
             + $x->[2]*($y->[0]*$z->[1] - $y->[1]*$z->[0]);
    }
    
    my %seen;
    foreach my $new_x (@axes) {
        foreach my $new_y (@axes) {
            # new_y must be orthogonal to new_x.
            next if dot($new_x, $new_y) != 0;
            my $new_z = cross($new_x, $new_y);
            # Ensure the determinant is 1 (proper rotation)
            next if det($new_x, $new_y, $new_z) != 1;
            # Represent the matrix as a string key to ensure uniqueness.
            my $mat_key = join(",", @$new_x, @$new_y, @$new_z);
            next if $seen{$mat_key}++;
            # Create closure for this rotation.
            push @rots, sub {
                my ($pt) = @_;
                return [
                    $pt->[0]*$new_x->[0] + $pt->[1]*$new_y->[0] + $pt->[2]*$new_z->[0],
                    $pt->[0]*$new_x->[1] + $pt->[1]*$new_y->[1] + $pt->[2]*$new_z->[1],
                    $pt->[0]*$new_x->[2] + $pt->[1]*$new_y->[2] + $pt->[2]*$new_z->[2],
                ];
            };
        }
    }
    return @rots;
}

my @rotations = generate_rotations();  # 24 rotation functions

# --- Parse the input file ---
my $input_file = "input.txt";
open my $fh, "<", $input_file or die "Cannot open $input_file: $!";
local $/ = undef;
my $contents = <$fh>;
close $fh;

# Each scanner section starts with a line like "--- scanner X ---"
my @scanner_blocks = split(/\n\n/, $contents);
my @scanners; 
foreach my $block (@scanner_blocks) {
    my @lines = split /\n/, $block;
    # skip the header line
    shift @lines;
    my @beacons;
    for my $line (@lines) {
        next unless $line =~ /\S/;
        my @coords = split /,/, $line;
        push @beacons, [ int($coords[0]), int($coords[1]), int($coords[2]) ];
    }
    push @scanners, { beacons => \@beacons };
}

# --- Alignment algorithm ---
# We'll treat scanner 0 as the reference (global coordinate system)
# and then iteratively find transformations for the other scanners.
my %global_beacons;  # global set of unique beacon positions (keys "x,y,z")
# also maintain an array (list) of global beacons to iterate over when matching
my @global_beacon_list;

# Mark scanner 0 as resolved: its position is [0,0,0] and its beacons are global.
$scanners[0]{pos} = [0,0,0];
for my $pt (@{ $scanners[0]{beacons} }) {
    my $key = pt2key($pt);
    unless (exists $global_beacons{$key}) {
        $global_beacons{$key} = 1;
        push @global_beacon_list, $pt;
    }
}

# Keep track of which scanners have been aligned.
my %resolved;
$resolved{0} = 1;

# Count of scanners
my $N = scalar @scanners;

# While there are scanners that are not resolved
while (scalar(keys %resolved) < $N) {
    my $found = 0;  # flag if in this iteration we resolved a new scanner
    # Try every unresolved scanner against every resolved scanner (via the global beacon set)
    for (my $i = 0; $i < $N; $i++) {
        next if exists $resolved{$i};
        # For scanner i, try every rotation
        SCANNER: for my $rot (@rotations) {
            # Apply current rotation to all of its beacons.
            my @rotated = map { $rot->($_) } @{ $scanners[$i]{beacons} };
            # We'll try to find, for each pair (rotated beacon, global beacon),
            # a translation that would match at least 12 beacons.
            my %translation_count;
            for my $pt (@rotated) {
                for my $g_key (@global_beacon_list) {
                    # Global beacon keys are in string form; convert back to array
                    my @g = split /,/, pt2key($pt); 
                    # Actually, we already have $pt, so for each global beacon in the global list:
                    # Instead, iterate over the global_beacon_list properly.
                    # (We rework the loops below for clarity.)
                    # We'll break out of this inner loop and iterate over global list:
                    # (So reorder loops.)
                }
            }
            # Instead, iterate over global beacon list:
            for my $g (@global_beacon_list) {
                for my $pt (@rotated) {
                    # compute translation vector t = global - rotated
                    my $t = sub { my ($a, $b) = @_; return $a - $b }->();  # dummy
                    my $t_vec = sub {
                        my ($gp, $pp) = @_;
                        return [ $gp->[0]-$pp->[0], $gp->[1]-$pp->[1], $gp->[2]-$pp->[2] ];
                    }->($g, $pt);
                    my $key_t = pt2key($t_vec);
                    $translation_count{$key_t}++;
                }
            }
            # Now check if any translation gives at least 12 matches.
            while (my ($t_key, $count) = each %translation_count) {
                if ($count >= 12) {
                    # Parse t_key back to a translation vector
                    my @t_coords = split /,/, $t_key;
                    my $translation = [ map { int($_) } @t_coords ];
                    # Apply rotation and translation on all scanner i beacons to get global coordinates.
                    my @transformed;
                    for my $pt (@rotated) {
                        my $glob = [ $pt->[0] + $translation->[0],
                                     $pt->[1] + $translation->[1],
                                     $pt->[2] + $translation->[2] ];
                        push @transformed, $glob;
                    }
                    # Update scanner i info.
                    $scanners[$i]{pos} = $translation;
                    $scanners[$i]{transformed} = \@transformed;
                    # Add any new beacons to the global set.
                    for my $pt (@transformed) {
                        my $key = pt2key($pt);
                        if (!exists $global_beacons{$key}) {
                            $global_beacons{$key} = 1;
                            push @global_beacon_list, $pt;
                        }
                    }
                    # Mark as resolved and break out.
                    $resolved{$i} = 1;
                    $found = 1;
                    last SCANNER;
                }
            }
        } # end for each rotation
    } # end for each unresolved scanner
    if (!$found) {
        die "No progress aligning remaining scanners! Something is wrong.\n";
    }
}

# --- Compute results ---

# Part One: count of unique beacons.
my $part1 = scalar( keys %global_beacons );

# Part Two: maximum Manhattan distance between any two scanners.
my @scanner_positions = map { $scanners[$_]{pos} } sort { $a <=> $b } keys %resolved;
my $max_distance = 0;
for (my $i = 0; $i < @scanner_positions; $i++) {
    for (my $j = $i+1; $j < @scanner_positions; $j++) {
        my $d = manhattan($scanner_positions[$i], $scanner_positions[$j]);
        $max_distance = $d if $d > $max_distance;
    }
}

# Print the answers.
print "Part One: $part1\n";
print "Part Two: $max_distance\n";

=pod

Explanation:

1. The script first generates the 24 possible rotations in 3D using an algorithm that picks orthogonal unit vectors.
2. It parses the input, assuming each scanner’s output is separated by a blank line and beacons are given as "x,y,z".
3. Scanner 0 is taken as the reference with position [0,0,0].
4. The script then iteratively aligns the other scanners by trying each rotation and computing a translation that causes at least 12 overlapping beacon points with those already placed.
5. Once aligned, it updates the global beacon set and stores each scanner's absolute position.
6. Finally, the script prints the number of unique beacons (Part One) and computes the largest Manhattan distance between any two scanner positions (Part Two).

To run this program, simply ensure your input is in a file called "input.txt" in the same directory and execute:

  perl this_script.pl

Enjoy! 

=cut

