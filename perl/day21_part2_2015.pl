#!/usr/bin/perl
use strict;
use warnings;

sub parse_stat {
    my ($line) = @_;
    my ($label, $value) = split /: /, $line;
    return $value;
}

sub max {
    my ($a, $b) = @_;
    return $a > $b ? $a : $b;
}

sub player_wins {
    my ($player, $boss) = @_;
    my $player_damage = max(1, $player->{Damage} - $boss->{Armor});
    my $boss_damage = max(1, $boss->{Damage} - $player->{Armor});

    my $player_turns = int(($boss->{HitPoints} + $player_damage - 1) / $player_damage);
    my $boss_turns = int(($player->{HitPoints} + $boss_damage - 1) / $boss_damage);

    return $player_turns <= $boss_turns;
}

open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my @lines = <$fh>;
close $fh;

chomp @lines;

my %boss = (
    HitPoints => parse_stat($lines[0]),
    Damage    => parse_stat($lines[1]),
    Armor     => parse_stat($lines[2]),
);

my @weapons = (
    { Cost => 8,  Damage => 4, Armor => 0 },
    { Cost => 10, Damage => 5, Armor => 0 },
    { Cost => 25, Damage => 6, Armor => 0 },
    { Cost => 40, Damage => 7, Armor => 0 },
    { Cost => 74, Damage => 8, Armor => 0 },
);

my @armors = (
    { Cost => 0,  Damage => 0, Armor => 0 },
    { Cost => 13, Damage => 0, Armor => 1 },
    { Cost => 31, Damage => 0, Armor => 2 },
    { Cost => 53, Damage => 0, Armor => 3 },
    { Cost => 75, Damage => 0, Armor => 4 },
    { Cost => 102, Damage => 0, Armor => 5 },
);

my @rings = (
    { Cost => 0,  Damage => 0, Armor => 0 },
    { Cost => 25, Damage => 1, Armor => 0 },
    { Cost => 50, Damage => 2, Armor => 0 },
    { Cost => 100, Damage => 3, Armor => 0 },
    { Cost => 20, Damage => 0, Armor => 1 },
    { Cost => 40, Damage => 0, Armor => 2 },
    { Cost => 80, Damage => 0, Armor => 3 },
);

my $max_cost = 0;
for my $w (@weapons) {
    for my $a (@armors) {
        for my $ri (0..$#rings) {
            for my $rj ($ri+1..$#rings) {
                my %player = (
                    HitPoints => 100,
                    Damage => $w->{Damage} + $rings[$ri]->{Damage} + $rings[$rj]->{Damage},
                    Armor => $a->{Armor} + $rings[$ri]->{Armor} + $rings[$rj]->{Armor},
                );
                my $cost = $w->{Cost} + $a->{Cost} + $rings[$ri]->{Cost} + $rings[$rj]->{Cost};
                if (!player_wins(\%player, \%boss) && $cost > $max_cost) {
                    $max_cost = $cost;
                }
            }
        }
    }
}

print "$max_cost\n";