
use strict;
use warnings;

my %valves;

open my $fh, '<', 'input.txt' or die $!;
my $input = do { local $/; <$fh> };
close $fh;

my @lines = split("\n", $input);
foreach my $line (@lines) {
    my @sp = split("; ", $line);
    my %v;
    ($v{id}, $v{flow}) = $sp[0] =~ /Valve (\w+) has flow rate=(\d+)/;
    $sp[1] = substr($sp[1], length("tunnel leads to valve"));
    if (index($sp[1], "s") == 0) {
        $sp[1] = substr($sp[1], 2);
    } else {
        $sp[1] = substr($sp[1], 1);
    }
    $v{tunnels} = {$v{id} => 0};
    foreach my $t (split(", ", $sp[1])) {
        $v{tunnels}{$t} = 1;
    }
    $valves{$v{id}} = \%v;
}

foreach my $k (keys %valves) {
    foreach my $i (keys %valves) {
        foreach my $j (keys %valves) {
            my ($dik, $okik) = ($valves{$i}{tunnels}{$k}, exists $valves{$i}{tunnels}{$k});
            my ($dkj, $okkj) = ($valves{$k}{tunnels}{$j}, exists $valves{$k}{tunnels}{$j});
            if ($okik && $okkj) {
                my ($dij, $okij) = ($valves{$i}{tunnels}{$j}, exists $valves{$i}{tunnels}{$j});
                if (!$okij || $dij > $dik + $dkj) {
                    $valves{$i}{tunnels}{$j} = $dik + $dkj;
                }
            }
        }
    }
}

my @open;
foreach my $v (values %valves) {
    if ($v->{flow} > 0) {
        push @open, $v->{id};
    }
}

print maxPressure(\%valves, "AA", 30, 0, \@open, 0) . "\n";

sub maxPressure {
    my ($valves, $curr, $minute, $pressure, $open, $d) = @_;
    my $max = $pressure;
    foreach my $next (@$open) {
        my @newopen = grep { $_ ne $next } @$open;
        my $timeLeft = $minute - $valves->{$curr}{tunnels}{$next} - 1;
        if ($timeLeft > 0) {
            $max = Max($max, maxPressure($valves, $next, $timeLeft, $timeLeft * $valves->{$next}{flow} + $pressure, \@newopen, $d + 1));
        }
    }
    return $max;
}

sub divide {
    my ($l) = @_;
    if ($l == 1) {
        return (
            [[], [0]],
            [[0], []]
        );
    }
    my @d = divide($l - 1);
    my @r;
    foreach my $i (0..$#d) {
        push @r, ([($l - 1, @{$d[$i][0]}), $d[$i][1]], [$d[$i][0], ($l - 1, @{$d[$i][1]})]);
    }
    return @r;
}

sub Max {
    my ($a, $b) = @_;
    return $a > $b ? $a : $b;
}
