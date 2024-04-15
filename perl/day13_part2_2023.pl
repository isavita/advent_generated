#!/usr/bin/perl

use strict;
use warnings;

sub min {
    my ($a, $b) = @_;
    return $a < $b ? $a : $b;
}

sub parse_mirror {
    my @mirror_str = @_;
    my $rows = scalar @mirror_str;
    my $cols = length $mirror_str[0];
    my @rows = (0) x $rows;
    my @cols = (0) x $cols;

    for my $y (0 .. $rows - 1) {
        for my $x (0 .. $cols - 1) {
            my $char = substr($mirror_str[$y], $x, 1);
            $rows[$y] <<= 1;
            $cols[$x] <<= 1;
            if ($char eq '#') {
                $rows[$y]++;
                $cols[$x]++;
            }
        }
    }

    return {
        rows => \@rows,
        cols => \@cols,
    };
}

sub get_mirror_axis_with_one_smudge {
    my @lines = @_;
    for my $i (1 .. $#lines) {
        my $is_mirror = 1;
        my $num_smudges = 0;

        for my $j (0 .. min($i - 1, $#lines - $i)) {
            if ($lines[$i - 1 - $j] != $lines[$i + $j]) {
                if ($num_smudges > 0) {
                    $is_mirror = 0;
                    last;
                } else {
                    my $dif = $lines[$i - 1 - $j] ^ $lines[$i + $j];
                    my $is_only_one_smudge = ($dif & ($dif - 1)) == 0;
                    if ($is_only_one_smudge) {
                        $num_smudges++;
                    } else {
                        $is_mirror = 0;
                        last;
                    }
                }
            }
        }

        if ($is_mirror && $num_smudges == 1) {
            return $i;
        }
    }

    return 0;
}

sub solve {
    my @input = @_;
    my @mirrors;
    my @mirror_str;

    for my $line (@input) {
        if ($line eq '') {
            push @mirrors, parse_mirror(@mirror_str);
            @mirror_str = ();
        } else {
            push @mirror_str, $line;
        }
    }
    push @mirrors, parse_mirror(@mirror_str);

    my $res = 0;
    for my $mirror (@mirrors) {
        $res += get_mirror_axis_with_one_smudge(@{$mirror->{cols}});
        $res += get_mirror_axis_with_one_smudge(@{$mirror->{rows}}) * 100;
    }
    return $res;
}

my $input_file = 'input.txt';
open my $fh, '<', $input_file or die "Could not open file '$input_file': $!";
my @input = <$fh>;
chomp @input;
close $fh;

my $result = solve(@input);
print "$result\n";