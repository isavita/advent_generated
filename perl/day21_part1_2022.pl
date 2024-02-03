
use strict;
use warnings;

my %jobs;
my %results;

open(my $fh, '<', 'input.txt') or die $!;

while (my $line = <$fh>) {
    chomp $line;
    my @parts = split(": ", $line);
    $jobs{$parts[0]} = $parts[1];
}

close($fh);

sub calculate {
    my ($monkey, $jobs, $results) = @_;

    return $results->{$monkey} if exists $results->{$monkey};

    die "Monkey not found: $monkey" unless exists $jobs->{$monkey};

    my $job = $jobs->{$monkey};

    if ($job =~ /^\d+$/) {
        $results->{$monkey} = $job;
        return $job;
    }

    my @parts = split(" ", $job);
    my $a = calculate($parts[0], $jobs, $results);
    my $b = calculate($parts[2], $jobs, $results);

    my $result;
    if ($parts[1] eq "+") {
        $result = $a + $b;
    } elsif ($parts[1] eq "-") {
        $result = $a - $b;
    } elsif ($parts[1] eq "*") {
        $result = $a * $b;
    } elsif ($parts[1] eq "/") {
        $result = $a / $b;
    } else {
        die "Unknown operation: $parts[1]";
    }

    $results->{$monkey} = $result;
    return $result;
}

print calculate("root", \%jobs, \%results) . "\n";
