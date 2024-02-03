
use strict;
use warnings;

my %programs;

sub dfs {
    my ($name, $programs) = @_;
    my $program = $programs->{$name};
    my $totalWeight = $program->{Weight};

    my %weights;
    foreach my $child (@{$program->{Holds}}) {
        my ($weight, $balanced) = dfs($child, $programs);
        if (!$balanced) {
            return (0, 0);
        }
        $totalWeight += $weight;
        $weights{$weight}++;
    }

    foreach my $w1 (keys %weights) {
        foreach my $w2 (keys %weights) {
            if ($w1 != $w2 && $weights{$w1} < $weights{$w2}) {
                my $unbalancedProgram = "";
                foreach my $child (@{$program->{Holds}}) {
                    my ($childWeight, $dummy) = dfs($child, $programs);
                    if ($childWeight == $w1) {
                        $unbalancedProgram = $child;
                        last;
                    }
                }
                print $programs->{$unbalancedProgram}->{Weight} + ($w2 - $w1) . "\n";
                return (0, 0);
            }
        }
    }
    return ($totalWeight, 1);
}

open my $fh, '<', 'input.txt' or die $!;
my @lines = <$fh>;
close $fh;

foreach my $line (@lines) {
    chomp $line;
    my @matches = $line =~ /[a-z]+|\d+/g;
    my $name = $matches[0];
    my $weight = $matches[1];

    my %program = (Weight => $weight);
    if (@matches > 2) {
        $program{Holds} = [@matches[2..$#matches]];
    }
    $programs{$name} = \%program;
}

my $root = "dtacyn"; # Replace this with the root found in Part One

dfs($root, \%programs);
