#!/usr/bin/perl
use strict;
use warnings;
use feature 'say';

my @keyPad = ("789","456","123"," 0A");
my @robotPad = (" ^A","<v>");
my $maxRobots = 3;
my %memo;

sub findPosition {
    my ($pad, $ch) = @_;
    for my $i (0 .. $#$pad) {
        for my $j (0 .. length($pad->[$i]) - 1) {
            return [$i, $j] if substr($pad->[$i], $j, 1) eq $ch;
        }
    }
    return [-1, -1];
}

sub ok {
    my ($pad, $pos, $seq) = @_;
    my ($i, $j) = @$pos;
    my $rows = scalar @$pad;
    my $cols = length($pad->[0]);
    for my $d (split //, $seq) {
        return 0 if substr($pad->[$i], $j, 1) eq ' ';
        $i--, next if $d eq '^';
        $i++, next if $d eq 'v';
        $j--, next if $d eq '<';
        $j++, next if $d eq '>';
        return 0;
        return 0 if $i < 0 || $i >= $rows || $j < 0 || $j >= $cols;
    }
    return 1;
}

sub generateMoves {
    my ($pos, $obj, $pad) = @_;
    my $objPos = findPosition($pad, $obj);
    my ($pi, $pj) = @$pos;
    my ($oi, $oj) = @$objPos;
    my $moves = "";
    $moves .= '<' x ($pj - $oj) if $pj > $oj;
    $moves .= '^' x ($pi - $oi) if $pi > $oi;
    $moves .= 'v' x ($oi - $pi) if $pi < $oi;
    $moves .= '>' x ($oj - $pj) if $pj < $oj;
    $moves = "" unless ok($pad, [$pi,$pj], $moves);
    if ($moves eq "") {
        $moves .= '>' x ($oj - $pj) if $pj < $oj;
        $moves .= '^' x ($pi - $oi) if $pi > $oi;
        $moves .= 'v' x ($oi - $pi) if $pi < $oi;
        $moves .= '<' x ($pj - $oj) if $pj > $oj;
    }
    return $moves;
}

sub solve {
    my ($code, $robots) = @_;
    return length($code) if $robots <= 0;
    my $key = "$robots|$code";
    return $memo{$key} if exists $memo{$key};
    my $ret = 0;
    my ($pi, $pj) = ($robots == $maxRobots) ? (3,2) : (0,2);
    for my $ch (split //, $code) {
        my $moves;
        if ($robots == $maxRobots) {
            $moves = generateMoves([$pi,$pj], $ch, \@keyPad);
            ($pi, $pj) = @{ findPosition(\@keyPad, $ch) };
        } else {
            $moves = generateMoves([$pi,$pj], $ch, \@robotPad);
            ($pi, $pj) = @{ findPosition(\@robotPad, $ch) };
        }
        $ret += solve($moves . "A", $robots - 1);
    }
    $memo{$key} = $ret;
    return $ret;
}

open my $fh, "<", "input.txt" or die "Error reading file: $!";
local $/;
my $content = <$fh>;
close $fh;
my $total = 0;
for my $line (split /\n/, $content) {
    $line =~ s/^\s+|\s+$//g;
    next unless length $line;
    my ($num) = $line =~ /(\d+)/;
    $total += solve($line, $maxRobots) * ($num // 0);
}
say $total;