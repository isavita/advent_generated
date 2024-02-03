
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my $input = do { local $/; <$fh> };
close $fh;

my %graph;
my @parsed = parseInput($input);

foreach my $pair (@parsed) {
    $graph{$pair->[0]} //= {};
    $graph{$pair->[1]} //= {};
    $graph{$pair->[0]}{$pair->[1]} = 1;
    $graph{$pair->[1]}{$pair->[0]} = 1;
}

my $result = walk(\%graph, "start", {"start" => 5}, ["start"], 0);
print "$result\n";

sub walk {
    my ($graph, $current, $visited, $path, $doubleUsed) = @_;

    return 1 if $current eq "end";

    $visited->{$current}++;

    my $pathsToEnd = 0;

    foreach my $visitable (keys %{$graph->{$current}}) {
        next if $visitable eq "start";

        if ($visitable ne uc $visitable && $visited->{$visitable} > 0) {
            if ($doubleUsed) {
                next;
            } else {
                $doubleUsed = 1;
            }
        }

        push @$path, $visitable;
        $pathsToEnd += walk($graph, $visitable, $visited, $path, $doubleUsed);

        $visited->{$visitable}--;
        pop @$path;

        if ($visitable ne uc $visitable && $visited->{$visitable} == 1) {
            $doubleUsed = 0;
        }
    }

    return $pathsToEnd;
}

sub parseInput {
    my ($input) = @_;
    my @ans;

    foreach my $line (split /\n/, $input) {
        push @ans, [split /-/, $line];
    }

    return @ans;
}
