use strict;
use warnings;

my @blueprints = parse_input();

my $sum = 0;
foreach my $bp (@blueprints) {
    my $st = new_state($bp);
    my $geodes_made = calc_most_geodes($st, 0, {}, 24, 24);
    $sum += $st->{blueprint}{id} * $geodes_made;
}

print $sum, "\n";

sub parse_input {
    open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
    my $input = do { local $/; <$fh> };
    close $fh;
    $input =~ s/\s+$//;

    my @ans;
    foreach my $line (split /\n/, $input) {
        my %bp;
        ($bp{id}, $bp{ore_for_ore_robot}, $bp{ore_for_clay_robot},
         $bp{ore_for_obsidian_robot}, $bp{clay_for_obsidian_robot},
         $bp{ore_for_geode_robot}, $bp{obsidian_for_geode_robot}) = $line =~
            /Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\./;
        push @ans, \%bp;
    }
    return @ans;
}

sub new_state {
    my ($blueprint) = @_;
    return {
        blueprint => $blueprint,
        ore_robots => 1,
        ore => 0,
        clay => 0,
        obsidian => 0,
        geode => 0,
        clay_robots => 0,
        obsidian_robots => 0,
        geode_robots => 0,
    };
}

sub farm {
    my ($s) = @_;
    $s->{ore} += $s->{ore_robots};
    $s->{clay} += $s->{clay_robots};
    $s->{obsidian} += $s->{obsidian_robots};
    $s->{geode} += $s->{geode_robots};
}

sub hash {
    my ($s, $time) = @_;
    return join ',', $time, @{$s}{qw(ore clay obsidian geode ore_robots clay_robots obsidian_robots geode_robots)};
}

sub copy {
    my ($s) = @_;
    return { %$s };
}

sub calc_most_geodes {
    my ($s, $time, $memo, $total_time, $earliest_geode) = @_;
    return $s->{geode} if $time == $total_time;

    my $h = hash($s, $time);
    return $memo->{$h} if exists $memo->{$h};

    return 0 if $s->{geode} == 0 && $time > $earliest_geode;

    my $most_geodes = $s->{geode};

    if ($s->{ore} >= $s->{blueprint}{ore_for_geode_robot} &&
        $s->{obsidian} >= $s->{blueprint}{obsidian_for_geode_robot}) {
        my $cp = copy($s);
        farm($cp);
        $cp->{ore} -= $cp->{blueprint}{ore_for_geode_robot};
        $cp->{obsidian} -= $cp->{blueprint}{obsidian_for_geode_robot};
        $cp->{geode_robots}++;
        $earliest_geode = $time + 1 if $cp->{geode_robots} == 1;
        $most_geodes = max($most_geodes, calc_most_geodes($cp, $time + 1, $memo, $total_time, $earliest_geode));
        $memo->{$h} = $most_geodes;
        return $most_geodes;
    }

    if ($time <= $total_time - 16 &&
        $s->{ore_robots} < $s->{blueprint}{ore_for_obsidian_robot} * 2 &&
        $s->{ore} >= $s->{blueprint}{ore_for_ore_robot}) {
        my $cp = copy($s);
        $cp->{ore} -= $cp->{blueprint}{ore_for_ore_robot};
        farm($cp);
        $cp->{ore_robots}++;
        $most_geodes = max($most_geodes, calc_most_geodes($cp, $time + 1, $memo, $total_time, $earliest_geode));
    }

    if ($time <= $total_time - 8 &&
        $s->{clay_robots} < $s->{blueprint}{clay_for_obsidian_robot} &&
        $s->{ore} >= $s->{blueprint}{ore_for_clay_robot}) {
        my $cp = copy($s);
        $cp->{ore} -= $cp->{blueprint}{ore_for_clay_robot};
        farm($cp);
        $cp->{clay_robots}++;
        $most_geodes = max($most_geodes, calc_most_geodes($cp, $time + 1, $memo, $total_time, $earliest_geode));
    }

    if ($time <= $total_time - 4 &&
        $s->{obsidian_robots} < $s->{blueprint}{obsidian_for_geode_robot} &&
        $s->{ore} >= $s->{blueprint}{ore_for_obsidian_robot} &&
        $s->{clay} >= $s->{blueprint}{clay_for_obsidian_robot}) {
        my $cp = copy($s);
        $cp->{ore} -= $cp->{blueprint}{ore_for_obsidian_robot};
        $cp->{clay} -= $cp->{blueprint}{clay_for_obsidian_robot};
        farm($cp);
        $cp->{obsidian_robots}++;
        $most_geodes = max($most_geodes, calc_most_geodes($cp, $time + 1, $memo, $total_time, $earliest_geode));
    }

    my $cp = copy($s);
    farm($cp);
    $most_geodes = max($most_geodes, calc_most_geodes($cp, $time + 1, $memo, $total_time, $earliest_geode));

    $memo->{$h} = $most_geodes;
    return $most_geodes;
}

sub max {
    my ($a, $b) = @_;
    return $a > $b ? $a : $b;
}