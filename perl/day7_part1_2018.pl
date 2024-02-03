
use strict;
use warnings;
use List::Util qw(uniq);

# Read and parse the input file
my ($deps, $allSteps) = parseInput("input.txt");
# Compute the order of steps
my $order = topologicalSort($deps, $allSteps);
print "$order\n";

sub parseInput {
    my ($filename) = @_;
    open(my $fh, '<', $filename) or die "Cannot open file: $!";
    
    my %deps;
    my %allSteps;
    
    while (my $line = <$fh>) {
        my ($a, $b) = $line =~ /Step (\w) must be finished before step (\w) can begin\./;
        push @{$deps{$b}}, $a;
        $allSteps{$a} = 1;
        $allSteps{$b} = 1;
    }
    
    close($fh);
    return (\%deps, \%allSteps);
}

sub topologicalSort {
    my ($deps, $allSteps) = @_;
    my @order;
    my @available;
    
    # Find initial available steps (with no dependencies)
    foreach my $step (keys %$allSteps) {
        if (!exists $deps->{$step} || scalar @{$deps->{$step}} == 0) {
            push @available, $step;
        }
    }
    @available = sort @available;

    while (scalar @available > 0) {
        # Pick the step that comes first alphabetically
        my $next = shift @available;
        push @order, $next;

        # Check and update the availability of the next steps
        foreach my $step (keys %$allSteps) {
            if (grep { $_ eq $next } @{$deps->{$step}}) {
                @{$deps->{$step}} = grep { $_ ne $next } @{$deps->{$step}};
                if (scalar @{$deps->{$step}} == 0) {
                    push @available, $step;
                }
            }
        }
        @available = sort @available;
    }
    return join('', @order);
}
