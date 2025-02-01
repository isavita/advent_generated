#!/usr/bin/perl
use strict;
use warnings;
use IO::File;

my %grid;
my @elves;
my $currDir = 0;
my @order = (1,5,7,3);
my @dirs = (
  [-1,-1], [-1,0], [-1,1], [0,1],
  [1,1], [1,0], [1,-1], [0,-1],
);

sub parse {
    my $fh = IO::File->new("input.txt", "r") or die $!;
    my $r = 0;
    while (<$fh>) {
        chomp;
        my @chars = split //;
        for my $c (0..$#chars) {
            if ($chars[$c] eq '#') {
                my $key = "$r,$c";
                $grid{$key} = 1;
                push @elves, { pos => [$r, $c], moving => 0, prop => undef };
            }
        }
        $r++;
    }
}

sub around_all_empty {
    my ($elf) = @_;
    my ($r, $c) = @{$elf->{pos}};
    for my $d (@dirs) {
        my $nr = $r + $d->[0];
        my $nc = $c + $d->[1];
        return 0 if exists $grid{"$nr,$nc"};
    }
    return 1;
}

sub elf_in_direction {
    my ($elf, $dir) = @_;
    my ($r, $c) = @{$elf->{pos}};
    for my $j (-1,0,1) {
        my $nd = ($dir + $j + 8) % 8;
        my $nr = $r + $dirs[$nd]->[0];
        my $nc = $c + $dirs[$nd]->[1];
        return 1 if exists $grid{"$nr,$nc"};
    }
    return 0;
}

sub run {
    my %proposals;
    for my $elf (@elves) {
        $elf->{moving} = 0;
        $elf->{prop} = undef;
        next if around_all_empty($elf);
        for my $i (0..3) {
            my $d = $order[($currDir+$i)%4];
            next if elf_in_direction($elf, $d);
            my ($r, $c) = @{$elf->{pos}};
            my $nr = $r + $dirs[$d]->[0];
            my $nc = $c + $dirs[$d]->[1];
            my $key = "$nr,$nc";
            $proposals{$key}++;
            $elf->{prop} = [$nr, $nc];
            $elf->{moving} = 1;
            last;
        }
    }
    my $moved = 0;
    for my $elf (@elves) {
        next unless $elf->{moving};
        my $key = join(",", @{$elf->{prop}});
        if ($proposals{$key} == 1) {
            my ($r, $c) = @{$elf->{pos}};
            delete $grid{"$r,$c"};
            $grid{$key} = 1;
            $elf->{pos} = [ split /,/, $key ];
            $moved = 1;
        }
    }
    $currDir = ($currDir+1) % 4;
    return $moved;
}

parse();
my $round = 0;
while (1) {
    last unless run();
    $round++;
}
print $round + 1, "\n";