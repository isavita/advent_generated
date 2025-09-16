#!/usr/bin/perl
use strict;
use warnings;

sub vec_add {
    my ($a, $b) = @_;
    [ $a->[0] + $b->[0], $a->[1] + $b->[1], $a->[2] + $b->[2] ]
}

sub vec_diff {
    my ($a, $b) = @_;
    [ $a->[0] - $b->[0], $a->[1] - $b->[1], $a->[2] - $b->[2] ]
}

sub vec_to_string {
    my ($p) = @_;
    "$p->[0],$p->[1],$p->[2]"
}

sub parse_input {
    my ($filename) = @_;
    my @scanners;
    my $current;
    open my $fh, '<', $filename or die "Could not open file: $filename";
    while (my $line = <$fh>) {
        chomp $line;
        if ($line =~ /^--- scanner \d+ ---$/) {
            $current = { beacons => [] };
            push @scanners, $current;
        } elsif ($line =~ /^(-?\d+),(-?\d+),(-?\d+)$/) {
            push @{ $current->{beacons} }, [ int($1), int($2), int($3) ];
        }
    }
    close $fh;
    return @scanners;
}

my @orients = (
    sub { my $p = shift; [ $p->[0],  $p->[1],  $p->[2] ] },
    sub { my $p = shift; [ $p->[0], -$p->[2],  $p->[1] ] },
    sub { my $p = shift; [ $p->[0], -$p->[1], -$p->[2] ] },
    sub { my $p = shift; [ $p->[0],  $p->[2], -$p->[1] ] },
    sub { my $p = shift; [-$p->[0], -$p->[1],  $p->[2] ] },
    sub { my $p = shift; [-$p->[0],  $p->[2],  $p->[1] ] },
    sub { my $p = shift; [-$p->[0],  $p->[1], -$p->[2] ] },
    sub { my $p = shift; [-$p->[0], -$p->[2], -$p->[1] ] },
    sub { my $p = shift; [  $p->[1], -$p->[0],  $p->[2] ] },
    sub { my $p = shift; [  $p->[1],  $p->[2],  $p->[0] ] },
    sub { my $p = shift; [  $p->[1],  $p->[0], -$p->[2] ] },
    sub { my $p = shift; [  $p->[1], -$p->[2], -$p->[0] ] },
    sub { my $p = shift; [-$p->[1],  $p->[0],  $p->[2] ] },
    sub { my $p = shift; [-$p->[1], -$p->[2],  $p->[0] ] },
    sub { my $p = shift; [-$p->[1], -$p->[0], -$p->[2] ] },
    sub { my $p = shift; [-$p->[1],  $p->[2], -$p->[0] ] },
    sub { my $p = shift; [  $p->[2],  $p->[1], -$p->[0] ] },
    sub { my $p = shift; [  $p->[2],  $p->[0],  $p->[1] ] },
    sub { my $p = shift; [  $p->[2], -$p->[1],  $p->[0] ] },
    sub { my $p = shift; [  $p->[2], -$p->[0], -$p->[1] ] },
    sub { my $p = shift; [-$p->[2], -$p->[1], -$p->[0] ] },
    sub { my $p = shift; [-$p->[2], -$p->[0],  $p->[1] ] },
    sub { my $p = shift; [-$p->[2],  $p->[1],  $p->[0] ] },
    sub { my $p = shift; [-$p->[2],  $p->[0], -$p->[1] ] },
);

sub try_match {
    my ($known_info, $unknown) = @_;
    my $known_beacons_abs = $known_info->{abs_beacons};
    my $known_beacons_abs_set = $known_info->{abs_beacons_set};
    my $unknown_beacons_relative = $unknown->{beacons};

    for my $ori_idx (0 .. $#orients) {
        my $orient = $orients[$ori_idx];
        for my $i (0 .. $#$known_beacons_abs) {
            my $known_b = $known_beacons_abs->[$i];
            for my $j (0 .. $#$unknown_beacons_relative) {
                my $oriented_unknown_b = $orient->($unknown_beacons_relative->[$j]);
                my $delta = vec_diff($known_b, $oriented_unknown_b);

                my $match_count = 0;
                for my $k (0 .. $#$unknown_beacons_relative) {
                    my $oriented_b_k = $orient->($unknown_beacons_relative->[$k]);
                    my $potential_abs_b = vec_add($oriented_b_k, $delta);
                    my $key = vec_to_string($potential_abs_b);
                    if (exists $known_beacons_abs_set->{$key}) {
                        $match_count++;
                    }
                }

                if ($match_count >= 12) {
                    my $unknown_scanner_abs_pos = $delta;

                    my @abs_beacons;
                    for my $k (0 .. $#$unknown_beacons_relative) {
                        my $oriented_b = $orient->($unknown_beacons_relative->[$k]);
                        my $abs_b = vec_add($oriented_b, $unknown_scanner_abs_pos);
                        push @abs_beacons, $abs_b;
                    }

                    return (1, $unknown_scanner_abs_pos, $ori_idx, \@abs_beacons);
                }
            }
        }
    }
    return (0);
}

sub main {
    my @scanners = parse_input("input.txt");
    my $num_scanners = scalar @scanners;

    my %all_beacons_set;
    my %scanner_info;
    my %located_indices;
    my @queue;

    # Initialize scanner 0 as reference
    $scanner_info{0} = {
        abs_pos => [0,0,0],
        orientation_idx => 0,
        abs_beacons => [],
        abs_beacons_set => {},
    };

    for my $beacon (@{ $scanners[0]{beacons} }) {
        my $abs_beacon = [ $beacon->[0], $beacon->[1], $beacon->[2] ];
        push @{ $scanner_info{0}{abs_beacons} }, $abs_beacon;
        my $key = vec_to_string($abs_beacon);
        $scanner_info{0}{abs_beacons_set}{$key} = 1;
        $all_beacons_set{$key} = 1;
    }

    $located_indices{0} = 1;
    push @queue, 0;
    my $located_count = 1;

    while (@queue && $located_count < $num_scanners) {
        my $known_idx = shift @queue;

        for my $unknown_idx (0 .. $num_scanners - 1) {
            next if $located_indices{$unknown_idx};

            my ($ok, $abs_pos, $ori_idx, $abs_beacons) = try_match($scanner_info{$known_idx}, $scanners[$unknown_idx]);
            if ($ok) {
                $located_indices{$unknown_idx} = 1;
                $located_count++;
                push @queue, $unknown_idx;

                my %abs_beacons_set;
                foreach my $be (@$abs_beacons) {
                    my $key = vec_to_string($be);
                    $abs_beacons_set{$key} = 1;
                    $all_beacons_set{$key} = 1;
                }

                $scanner_info{$unknown_idx} = {
                    abs_pos => $abs_pos,
                    orientation_idx => $ori_idx,
                    abs_beacons => $abs_beacons,
                    abs_beacons_set => \%abs_beacons_set,
                };
            }
        }
    }

    my $unique_beacon_count = scalar keys %all_beacons_set;
    print "$unique_beacon_count\n";
}

main();