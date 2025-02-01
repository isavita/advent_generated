#!/usr/bin/perl
use strict;
use warnings;
use 5.010;
local $/ = undef;
my $data = do { open my $fh, "<", "input.txt" or die $!; <$fh> };
my @lines = split /\n/, $data;
my @groups;
my $army;
my %army_id = ( "Immune System" => 1, "Infection" => 2 );
for (@lines) {
    s/^\s+|\s+$//g;
    next unless length;
    if (/^([^:]+):$/) {
        $army = $army_id{$1};
    } else {
        if (/^(\d+)\s+units each with\s+(\d+)\s+hit points(?:\s+\(([^)]+)\))?\s+with an attack that does\s+(\d+)\s+(\S+)\s+damage at initiative\s+(\d+)$/) {
            my ($units, $hp, $info, $dmg, $type, $init) = ($1, $2, $3, $4, $5, $6);
            my (@imm, @weak);
            if (defined $info) {
                if ($info =~ /immune to ([^;]+)/) {
                    @imm = split /,\s*/, $1;
                }
                if ($info =~ /weak to ([^;]+)/) {
                    @weak = split /,\s*/, $1;
                }
            }
            push @groups, { units => $units, hp => $hp, dmg => $dmg, type => $type, init => $init, imm => \@imm, weak => \@weak, army => $army, target => undef, chosen => 0 };
        }
    }
}
sub effective { $_[0]{units} * $_[0]{dmg} }
sub damage {
    my ($att, $def) = @_;
    return 0 if grep { $_ eq $att->{type} } @{$def->{imm}};
    my $d = effective($att);
    $d *= 2 if grep { $_ eq $att->{type} } @{$def->{weak}};
    return $d;
}
my $prev_total = -1;
while (1) {
    my @alive = grep { $_->{units} > 0 } @groups;
    my %armies;
    $armies{$_->{army}}++ for @alive;
    last if scalar(keys %armies) < 2;
    for (@alive) { $_->{target} = undef; $_->{chosen} = 0 }
    my @select = sort { 
         effective($b) <=> effective($a) || $b->{init} <=> $a->{init} 
    } @alive;
    for my $att (@select) {
        my ($best, $best_d, $best_ep, $best_init) = (undef, 0, 0, 0);
        for my $def (grep { $_->{army} != $att->{army} && $_->{units} > 0 && !$_->{chosen} } @alive) {
            my $d = damage($att, $def);
            next if $d == 0;
            my $ep = effective($def);
            if ($d > $best_d || ($d == $best_d && ($ep > $best_ep || ($ep == $best_ep && $def->{init} > $best_init)))) {
                ($best, $best_d, $best_ep, $best_init) = ($def, $d, $ep, $def->{init});
            }
        }
        if ($best) {
            $att->{target} = $best;
            $best->{chosen} = 1;
        }
    }
    my $total_killed = 0;
    my @attack = sort { $b->{init} <=> $a->{init} } @alive;
    for my $att (@attack) {
        next unless $att->{units} > 0 && defined $att->{target} && $att->{target}{units} > 0;
        my $d = damage($att, $att->{target});
        my $k = int($d / $att->{target}{hp});
        $k = $att->{target}{units} if $k > $att->{target}{units};
        $att->{target}{units} -= $k;
        $total_killed += $k;
        $att->{target} = undef;
    }
    last if $total_killed == 0;
    my $curr_total = 0;
    $curr_total += $_->{units} for grep { $_->{units} > 0 } @groups;
    last if $curr_total == $prev_total;
    $prev_total = $curr_total;
}
my $res = 0;
$res += $_->{units} for grep { $_->{units} > 0 } @groups;
say $res;