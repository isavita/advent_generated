use strict;
use warnings;
use List::Util qw(min);

my $filename = "input.txt";
open my $fh, '<', $filename or die "Cannot open file: $!";
my %deps;
my %allSteps;

while (my $line = <$fh>) {
    chomp $line;
    my ($a, $b) = $line =~ /Step (\w) must be finished before step (\w) can begin/;
    push @{$deps{$b} //= []}, $a;
    $allSteps{$a} //= { id => $a, duration => ord($a) - ord('A') + 61 };
    $allSteps{$b} //= { id => $b, duration => ord($b) - ord('A') + 61 };
}

my $numWorkers = 5;
my $baseDuration = 60;
my @workers = (0) x $numWorkers;
my @tasks = (0) x $numWorkers;
my $time = 0;

while (keys %allSteps) {
    my @available;
    for my $step (keys %allSteps) {
        if (!@{$deps{$step} // []} && !isBeingWorkedOn($step, \@tasks)) {
            push @available, $step;
        }
    }
    @available = sort @available;

    for my $i (0..$numWorkers-1) {
        if ($workers[$i] == 0 && @available) {
            $tasks[$i] = $available[0];
            $workers[$i] = $allSteps{$available[0]}{duration};
            shift @available;
        }
    }

    my $minDuration = min(grep {$_ > 0} @workers);
    for my $i (0..$numWorkers-1) {
        if ($workers[$i] != 0) {
            $workers[$i] -= $minDuration;
            if ($workers[$i] == 0) {
                finishStep(\%deps, \%allSteps, $tasks[$i]);
                $tasks[$i] = 0;
            }
        }
    }
    $time += $minDuration;
}

print $time . "\n";

sub isBeingWorkedOn {
    my ($step, $tasks) = @_;
    foreach my $task (@$tasks) {
        if ($task && $task eq $step) {
            return 1;
        }
    }
    return 0;
}

sub finishStep {
    my ($deps, $allSteps, $step) = @_;
    delete $allSteps->{$step};
    foreach my $s (keys %$allSteps) {
        $deps->{$s} = [ grep {$_ ne $step} @{$deps->{$s}} ];
    }
}