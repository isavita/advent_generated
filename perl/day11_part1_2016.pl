
use strict;
use warnings;
use List::Util qw(first);
use Storable qw(dclone);
use Data::Dumper;

my $input = read_file("input.txt");
my $ans = rtg_hell_day($input);
print "$ans\n";

sub rtg_hell_day {
    my $input = shift;
    my $current_state = new_initial_state($input);
    my @queue = ($current_state);
    my %prev_states;

    while (@queue) {
        my $front = shift @queue;
        return $front->{steps} if is_done($front);

        my $hash = hash_key($front);
        next if $prev_states{$hash}++;
        
        push @queue, get_next_states($front);
    }
    return -1;
}

sub new_initial_state {
    my $input = shift;
    my @floors;
    my @lines = split /\n/, $input;

    for my $line_index (0..$#lines) {
        my @parts = split /\s+/, $lines[$line_index];
        for my $i (0..$#parts) {
            $parts[$i] =~ s/[,.]//g;
            if ($parts[$i] eq 'generator') {
                push @{$floors[$line_index]}, { isChip => 0, material => $parts[$i-1] };
            } elsif ($parts[$i] eq 'microchip') {
                push @{$floors[$line_index]}, { isChip => 1, material => substr($parts[$i-1], 0, index($parts[$i-1], '-comp')) };
            }
        }
    }
    return { floors => \@floors, elevatorLevel => 0, steps => 0 };
}

sub is_done {
    my $state = shift;
    return sum(map { scalar @{$state->{floors}[$_]} } 0..2) == 0;
}

sub hash_key {
    my $state = shift;
    my (%gen, %chip);
    for my $fl_index (0..$#{$state->{floors}}) {
        for my $half (@{$state->{floors}[$fl_index]}) {
            if ($half->{isChip}) {
                $chip{$half->{material}} = $fl_index;
            } else {
                $gen{$half->{material}} = $fl_index;
            }
        }
    }
    my @pairs = map { [$gen{$_}, $chip{$_}] } keys %gen;
    return join(',', $state->{elevatorLevel}, sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @pairs);
}

sub get_next_states {
    my $state = shift;
    my @future_states;
    my @movable_indices = get_movable_perm_indices($state);
    my @ele_diffs = ($state->{elevatorLevel} < 3 ? 1 : (), $state->{elevatorLevel} > 0 ? -1 : ());

    for my $ele_diff (@ele_diffs) {
        for my $perm_indices (@movable_indices) {
            my $cl = dclone($state);
            $cl->{elevatorLevel} += $ele_diff;
            $cl->{steps}++;
            my $old_level = $state->{elevatorLevel};
            my $new_level = $cl->{elevatorLevel};

            push @{$cl->{floors}[$new_level]}, delete $cl->{floors}[$old_level]->[$_] for @$perm_indices;
            next unless is_valid($cl);
            push @future_states, $cl;
        }
    }
    return @future_states;
}

sub get_movable_perm_indices {
    my $state = shift;
    my @perms_to_move;
    my $current_level = $state->{floors}[$state->{elevatorLevel}];

    for my $i (0..$#$current_level) {
        for my $j ($i+1..$#$current_level) {
            push @perms_to_move, [$i, $j];
        }
        push @perms_to_move, [$i];
    }
    return @perms_to_move;
}

sub is_valid {
    my $state = shift;
    for my $i (0..$#{$state->{floors}}) {
        my %gens_seen;
        for my $half (@{$state->{floors}[$i]}) {
            $gens_seen{$half->{material}}++ unless $half->{isChip};
        }
        return 0 if any { $_->{isChip} && !$gens_seen{$_->{material}} } @{$state->{floors}[$i]};
    }
    return 1;
}

sub read_file {
    my $filename = shift;
    open my $fh, '<', $filename or die $!;
    local $/;
    return <$fh>;
}

sub sum { my $s = 0; $s += $_ for @_; return $s; }
sub any { my $cond = shift; return scalar grep { $cond->($_) } @_; }
