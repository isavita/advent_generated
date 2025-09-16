#!/usr/bin/perl
use strict;
use warnings;

package Group;
sub new {
    my ($class, $units, $hit_points, $attack_damage, $attack_type, $initiative, $immunities, $weaknesses, $army_id, $id) = @_;
    my $o = bless {
        units => $units,
        hit_points => $hit_points,
        attack_damage => $attack_damage,
        attack_type => $attack_type // "",
        initiative => $initiative,
        immunities => $immunities // {},
        weaknesses => $weaknesses // {},
        target => undef,
        army_id => $army_id,
        id => $id
    }, $class;
    return $o;
}
sub effective_power {
    my ($g) = @_;
    return $g->{units} * $g->{attack_damage};
}
sub damage_dealt {
    my ($g, $ enemy) = @_;
    return 0 if $g->{units} <= 0;
    return 0 if exists $enemy->{immunities}{$g->{attack_type}};
    my $power = $g->effective_power;
    if (exists $enemy->{weaknesses}{$g->{attack_type}}) {
        return $power * 2;
    }
    return $power;
}

package Army;
sub new {
    my ($class, $id) = @_;
    bless { id => $id, groups => [] }, $class;
}
sub add_group {
    my ($o, $group) = @_;
    push @{$o->{groups}}, $group;
}
sub alive {
    my ($o) = @_;
    foreach my $g (@{$o->{groups}}) {
        return 1 if $g->{units} > 0;
    }
    return 0;
}
sub boost {
    my ($o, $amount) = @_;
    foreach my $g (@{$o->{groups}}) {
        $g->{attack_damage} += $amount;
    }
}
sub total_units {
    my ($o) = @_;
    my $sum = 0;
    foreach my $g (@{$o->{groups}}) {
        $sum += $g->{units} if $g->{units} > 0;
    }
    return $sum;
}

package Battlefield;
sub new {
    my ($class, %args) = @_;
    bless { armies => $args{armies} }, $class;
}
sub get_all_groups {
    my ($self) = @_;
    my @all;
    foreach my $army_id (keys %{$self->{armies}}) {
        my $army = $self->{armies}{$army_id};
        foreach my $g (@{$army->{groups}}) {
            push @all, $g;
        }
    }
    return @all;
}
sub find_targets {
    my ($self) = @_;
    my @potential_attackers;
    my %potential_targets;

    foreach my $army_id (keys %{$self->{armies}}) {
        my $army = $self->{armies}{$army_id};
        foreach my $g (@{$army->{groups}}) {
            $g->{target} = undef;
            if ($g->{units} > 0) {
                push @potential_attackers, $g;
                $potential_targets{$g} = 1;
            }
        }
    }

    @potential_attackers = sort {
        ( $b->{units} * $b->{attack_damage} ) <=> ( $a->{units} * $a->{attack_damage} ) ||
        ( $b->{initiative} <=> $a->{initiative} )
    } @potential_attackers;

    foreach my $attacker (@potential_attackers) {
        my $best_target;
        my $max_damage = -1;
        my $enemy_army_id = ($attacker->{army_id} == 1) ? 2 : 1;
        my $enemy_army = $self->{armies}{$enemy_army_id};
        if ($enemy_army) {
            foreach my $enemy (@{$enemy_army->{groups}}) {
                if ($enemy->{units} > 0 && exists $potential_targets{$enemy}) {
                    my $damage = $attacker->damage_dealt($enemy);
                    if ($damage > 0) {
                        if ($damage > $max_damage) {
                            $max_damage = $damage;
                            $best_target = $enemy;
                        } elsif ($damage == $max_damage) {
                            my $enemy_ep = $enemy->{units} * $enemy->{attack_damage};
                            my $current_ep = $best_target->{units} * $best_target->{attack_damage};
                            if ($enemy_ep > $current_ep) {
                                $best_target = $enemy;
                            } elsif ($enemy_ep == $current_ep && $enemy->{initiative} > $best_target->{initiative}) {
                                $best_target = $enemy;
                            }
                        }
                    }
                }
            }
        }

        if (defined $best_target) {
            $attacker->{target} = $best_target;
            delete $potential_targets{$best_target};
        }
    }
}
sub attack_phase {
    my ($self) = @_;
    my @attackers;
    foreach my $army_id (keys %{$self->{armies}}) {
        my $army = $self->{armies}{$army_id};
        foreach my $g (@{$army->{groups}}) {
            push @attackers, $g if $g->{units} > 0;
        }
    }

    @attackers = sort { $b->{initiative} <=> $a->{initiative} } @attackers;

    my $total_units_killed = 0;
    foreach my $attacker (@attackers) {
        next if $attacker->{units} <= 0;
        next unless defined $attacker->{target};
        my $target = $attacker->{target};
        next if $target->{units} <= 0;
        my $damage = $attacker->damage_dealt($target);
        my $units_killed = int($damage / $target->{hit_points});
        $units_killed = $target->{units} if $units_killed > $target->{units};
        $target->{units} -= $units_killed;
        $total_units_killed += $units_killed;
    }
    return $total_units_killed > 0;
}
sub clean {
    my ($self) = @_;
    foreach my $army_id (keys %{$self->{armies}}) {
        my $army = $self->{armies}{$army_id};
        my @living;
        foreach my $g (@{$army->{groups}}) {
            push @living, $g if $g->{units} > 0;
        }
        $army->{groups} = \@living;
    }
}
sub active {
    my ($self) = @_;
    my $alive_count = 0;
    foreach my $army_id (keys %{$self->{armies}}) {
        my $army = $self->{armies}{$army_id};
        $alive_count++ if $army->alive;
    }
    return $alive_count == 2;
}
sub result {
    my ($self) = @_;
    my $winner = 0;
    my $units = 0;
    foreach my $army_id (keys %{$self->{armies}}) {
        my $army = $self->{armies}{$army_id};
        if ($army->alive) {
            $winner = $army->{id};
            $units = $army->total_units;
            last;
        }
    }
    return ($winner, $units);
}
sub total_units {
    my ($self) = @_;
    my $count = 0;
    foreach my $army_id (keys %{$self->{armies}}) {
        $count += $self->{armies}{$army_id}->total_units;
    }
    return $count;
}

package main;
sub parse_input {
    my ($input_data) = @_;
    my %armies;
    $armies{1} = Army->new(1);
    $armies{2} = Army->new(2);
    my $current_army = 0;
    my $group_counter = 0;
    for my $line (split /\r?\n/, $input_data) {
        $line =~ s/^\s+|\s+$//g;
        if ($line eq "Immune System:") {
            $current_army = 1;
        } elsif ($line eq "Infection:") {
            $current_army = 2;
        } elsif ($line eq "") {
            next;
        } else {
            if ($line =~ /^(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)$/) {
                my ($units, $hp, $ad, $attack_type, $initiative) = ($1, $2, $3, $4, $5);
                my ($modifiers) = $line =~ /\(([^)]+)\)/;
                my %immune;
                my %weak;
                if (defined $modifiers) {
                    if ($modifiers =~ /immune to ([^);]+)(?:[);]|$)/) {
                        my $s = $1;
                        for my $tok (split /\s*,\s*/, $s) { $immune{$tok} = 1; }
                    }
                    if ($modifiers =~ /weak to ([^);]+)(?:[);]|$)/) {
                        my $s = $1;
                        for my $tok (split /\s*,\s*/, $s) { $weak{$tok} = 1; }
                    }
                }
                $group_counter++;
                my $group = Group->new($units, $hp, $ad, $attack_type, $initiative, \%immune, \%weak, $current_army, $group_counter);
                $armies{$current_army}->add_group($group);
            }
        }
    }
    return Battlefield->new(armies => \%armies);
}
sub run_simulation {
    my ($input_data, $boost) = @_;
    my $battle = parse_input($input_data);
    $battle->{armies}{1}->boost($boost);
    my $round = 0;
    my $max_rounds = 5000;
    while ($battle->active() && $round < $max_rounds) {
        $round++;
        my $units_before = $battle->total_units();
        $battle->find_targets();
        $battle->attack_phase();
        $battle->clean();
        my $units_after = $battle->total_units();
        if ($units_before == $units_after) {
            return (0, 0, 1);
        }
    }
    my ($winner, $units) = $battle->result();
    my $stalemate = ($round == $max_rounds && $battle->active()) ? 1 : 0;
    return ($winner, $units, $stalemate);
}
sub find_min_boost {
    my ($input_data) = @_;
    my $boost = 0;
    while (1) {
        my ($winner, $units, $stalemate) = run_simulation($input_data, $boost);
        if (!$stalemate && $winner == 1) {
            return $units;
        }
        $boost++;
        if ($boost > 1000) {
            die "Boost exceeded limit, likely impossible or error in logic.\n";
        }
    }
}
sub main {
    my $filename = "input.txt";
    open my $fh, "<", $filename or die "Error: Cannot open input.txt\n";
    my $input_data = do { local $/; <$fh> };
    close $fh;
    $input_data =~ s/^\s+|\s+$//g;
    my $result = find_min_boost($input_data);
    print "$result\n";
}
main();