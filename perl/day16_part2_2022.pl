#!/usr/bin/perl
use strict;
use warnings;

my $INF = 1_000_000;
my %idx;
my @valve_id;
my @flow;
my %dist;

sub add_valve {
    my ($id) = @_;
    unless (exists $idx{$id}) {
        my $i = scalar @valve_id;
        $idx{$id} = $i;
        push @valve_id, $id;
        push @flow, 0;
        $dist{$i} ||= {};
        $dist{$i}{$i} = 0;
    }
    return $idx{$id};
}
sub dist_get { my ($i,$j) = @_; return (exists $dist{$i} && exists $dist{$i}{$j}) ? $dist{$i}{$j} : $INF; }
sub dist_set { my ($i,$j,$v) = @_; $dist{$i}{$j} = $v; }

open my $fh, '<', 'input.txt' or die "Can't open input.txt: $!";
while (my $line = <$fh>) {
    chomp $line;
    next if $line =~ /^\s*$/;
    if ($line =~ /^Valve\s+([A-Z]{2})\s+has\s+flow\s+rate=(\d+);.*?\bvalves?\s+(.*)$/i) {
        my ($id, $flow, $nb) = ($1, $2, $3);
        my $cur = add_valve($id);
        $flow[$cur] = int($flow);
        my @nbs = split /\s*,\s*/, $nb;
        foreach my $nb_id (@nbs) {
            next if $nb_id eq '';
            my $nidx = add_valve($nb_id);
            dist_set($cur, $nidx, 1);
            dist_set($nidx, $cur, 1);
        }
    }
}
close $fh;

my $n = scalar @valve_id;

# Floyd-Warshall
for (my $k = 0; $k < $n; $k++) {
    for (my $i = 0; $i < $n; $i++) {
        my $dik = dist_get($i, $k);
        next if $dik >= $INF;
        for (my $j = 0; $j < $n; $j++) {
            my $dkj = dist_get($k, $j);
            next if $dkj >= $INF;
            my $dij = dist_get($i, $j);
            my $cand = $dik + $dkj;
            if ($cand < $dij) {
                dist_set($i, $j, $cand);
            }
        }
    }
}

my @open_indices;
for (my $i = 0; $i < $n; $i++) {
    push @open_indices, $i if $flow[$i] > 0;
}
my $num_open = scalar @open_indices;

my $start_idx = defined $idx{'AA'} ? $idx{'AA'} : -1;
die "Start valve 'AA' not found.\n" if $start_idx == -1;

my %memo;
sub solve {
    my ($cur, $time_left, $mask) = @_;
    return 0 if $time_left <= 0;
    my $key = $cur."|".$time_left."|".$mask;
    return $memo{$key} if exists $memo{$key};

    my $best = 0;
    for (my $i = 0; $i < $num_open; $i++) {
        if ((($mask >> $i) & 1) == 1) {
            my $target = $open_indices[$i];
            my $travel = dist_get($cur, $target);
            my $remain = $time_left - $travel - 1;
            if ($remain > 0) {
                my $cur_pressure = $flow[$target] * $remain;
                my $new_mask = $mask & ~(1 << $i);
                my $val = $cur_pressure + solve($target, $remain, $new_mask);
                $best = $val if $val > $best;
            }
        }
    }
    $memo{$key} = $best;
    return $best;
}

my $TIME_LIMIT = 26;
my $best = 0;
my $total_masks = 1 << $num_open;

for (my $my_mask = 0; $my_mask < $total_masks; $my_mask++) {
    my $elephant_mask = ($total_masks - 1) ^ $my_mask;
    my $my_p = solve($start_idx, $TIME_LIMIT, $my_mask);
    my $ele_p = solve($start_idx, $TIME_LIMIT, $elephant_mask);
    my $sum = $my_p + $ele_p;
    $best = $sum if $sum > $best;
}

print $best, "\n";