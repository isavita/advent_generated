
use strict;
use warnings;

my @rules;
my @myTicket;
my @nearbyTickets;
my $section = 0;
my $reRule = qr/^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$/;
my %validPositions;

open(my $fh, '<', 'input.txt') or die "File reading error: $!";
while (my $line = <$fh>) {
    chomp $line;
    if ($line eq "") {
        $section++;
        next;
    }
    if ($section == 0) {
        if (my @parts = $line =~ /$reRule/) {
            my $rule = {
                name => $parts[0],
                ranges => [
                    [$parts[1], $parts[2]],
                    [$parts[3], $parts[4]]
                ]
            };
            push @rules, $rule;
        }
    } elsif ($section == 1) {
        if ($line ne "your ticket:") {
            @myTicket = parseTicket($line);
        }
    } elsif ($section == 2) {
        if ($line ne "nearby tickets:") {
            my @ticket = parseTicket($line);
            if (isValidTicket(\@ticket, \@rules)) {
                push @nearbyTickets, \@ticket;
            }
        }
    }
}
close($fh);

my %fieldPositions = solveFieldPositions(\@rules, \@nearbyTickets);
my $departureProduct = calculateDepartureProduct(\@myTicket, \%fieldPositions);

print "$departureProduct\n";

sub toInt {
    my $s = shift;
    return int($s);
}

sub parseTicket {
    my $s = shift;
    my @values = split(",", $s);
    my @ticket;
    foreach my $v (@values) {
        push @ticket, toInt($v);
    }
    return @ticket;
}

sub isValidTicket {
    my ($ticket, $rules) = @_;
    foreach my $value (@$ticket) {
        if (!isValidForAnyRule($value, $rules)) {
            return 0;
        }
    }
    return 1;
}

sub isValidForAnyRule {
    my ($value, $rules) = @_;
    foreach my $rule (@$rules) {
        if ($rule->{ranges}[0][0] <= $value && $value <= $rule->{ranges}[0][1] ||
            $rule->{ranges}[1][0] <= $value && $value <= $rule->{ranges}[1][1]) {
            return 1;
        }
    }
    return 0;
}

sub solveFieldPositions {
    my ($rules, $tickets) = @_;
    my %validPositions;
    foreach my $rule (@$rules) {
        $validPositions{$rule->{name}} = {};
        for (my $i = 0; $i < scalar(@{$tickets->[0]}); $i++) {
            my $valid = 1;
            foreach my $ticket (@$tickets) {
                if (!isValidForRule($ticket->[$i], $rule)) {
                    $valid = 0;
                    last;
                }
            }
            if ($valid) {
                $validPositions{$rule->{name}}{$i} = 1;
            }
        }
    }

    my %fieldPositions;
    while (scalar(keys %fieldPositions) < scalar(@$rules)) {
        foreach my $name (keys %validPositions) {
            if (scalar(keys %{$validPositions{$name}}) == 1) {
                my $pos = (keys %{$validPositions{$name}})[0];
                $fieldPositions{$name} = $pos;
                foreach my $otherName (keys %validPositions) {
                    delete $validPositions{$otherName}{$pos};
                }
                delete $validPositions{$name};
            }
        }
    }
    return %fieldPositions;
}

sub isValidForRule {
    my ($value, $rule) = @_;
    return ($rule->{ranges}[0][0] <= $value && $value <= $rule->{ranges}[0][1] ||
            $rule->{ranges}[1][0] <= $value && $value <= $rule->{ranges}[1][1]);
}

sub calculateDepartureProduct {
    my ($ticket, $fieldPositions) = @_;
    my $product = 1;
    foreach my $name (keys %$fieldPositions) {
        if ($name =~ /^departure/) {
            $product *= $ticket->[$fieldPositions->{$name}];
        }
    }
    return $product;
}
