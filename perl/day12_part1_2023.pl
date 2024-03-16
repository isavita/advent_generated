#!/usr/bin/perl

use strict;
use warnings;

sub parse_input {
    my $file = shift;
    open my $fh, '<', $file or die $!;
    my @lines = <$fh>;
    close $fh;
    chomp @lines;
    return @lines;
}

sub parse_string_to_ints {
    my $numbers_line = shift;
    my @numbers = split(',', $numbers_line);
    return map { int($_) } @numbers;
}

sub count_arrangements_recursive {
    my ($row, $i_springs, $i_group, $i_contiguous_damaged, $cache) = @_;

    if ($i_springs == length($row->{springs})) {
        if ($i_group == scalar(@{$row->{group}}) && $i_contiguous_damaged == 0) {
            return 1;
        } elsif ($i_group == scalar(@{$row->{group}}) - 1 && $i_contiguous_damaged == $row->{group}[$i_group]) {
            return 1;
        }
        return 0;
    }

    my $cache_key = "$i_springs,$i_group,$i_contiguous_damaged";
    if (exists $cache->{$cache_key}) {
        return $cache->{$cache_key};
    }

    my $res = 0;
    my $char = substr($row->{springs}, $i_springs, 1);
    if ($char eq '.' || $char eq '?') {
        if ($i_contiguous_damaged == 0) {
            $res += count_arrangements_recursive($row, $i_springs+1, $i_group, $i_contiguous_damaged, $cache);
        } elsif ($i_contiguous_damaged == $row->{group}[$i_group]) {
            $res += count_arrangements_recursive($row, $i_springs+1, $i_group+1, 0, $cache);
        }
    }
    if ($char eq '#' || $char eq '?') {
        if ($i_group < scalar(@{$row->{group}}) && $i_contiguous_damaged < $row->{group}[$i_group]) {
            $res += count_arrangements_recursive($row, $i_springs+1, $i_group, $i_contiguous_damaged+1, $cache);
        }
    }

    $cache->{$cache_key} = $res;
    return $res;
}

sub count_arrangements {
    my $row = shift;
    return count_arrangements_recursive($row, 0, 0, 0, {});
}

sub solve {
    my @input = @_;
    my @rows;
    foreach my $line (@input) {
        my ($springs, $numbers_line) = split(' ', $line);
        my @group = parse_string_to_ints($numbers_line);
        push @rows, {springs => $springs, group => \@group};
    }
    my $res = 0;
    foreach my $row (@rows) {
        $res += count_arrangements($row);
    }
    return $res;
}

my @input = parse_input("input.txt");
my $result = solve(@input);
print "$result\n";