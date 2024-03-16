#!/usr/bin/perl

use strict;
use warnings;

package SnailNumber;

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    
    $self->{Value} = shift;
    $self->{Left} = undef;
    $self->{Right} = undef;

    return $self;
}

sub IsRegular {
    my $self = shift;
    return !($self->{Left} || $self->{Right});
}

sub Add {
    my ($self, $other) = @_;
    my $newNumber = bless { Left => $self, Right => $other }, 'SnailNumber';
    return $newNumber->Reduce();
}

sub Reduce {
    my $self = shift;
    while (1) {
        my ($exploded) = $self->Explode(0);
        if ($exploded) {
            next;
        }
        if (!$self->Split()) {
            last;
        }
    }
    return $self;
}

sub Explode {
    my ($self, $depth) = @_;
    if ($self->IsRegular()) {
        return (0, 0, 0);
    }

    if ($depth == 4) {
        my $leftValue = $self->{Left}->{Value};
        my $rightValue = $self->{Right}->{Value};
        $self->{Left} = undef;
        $self->{Right} = undef;
        $self->{Value} = 0;
        return (1, $leftValue, $rightValue);
    }

    my ($exploded, $leftValue, $rightValue) = $self->{Left}->Explode($depth + 1);
    if ($exploded) {
        if ($rightValue > 0 && $self->{Right}) {
            $self->{Right}->AddLeft($rightValue);
        }
        return (1, $leftValue, 0);
    }

    ($exploded, $leftValue, $rightValue) = $self->{Right}->Explode($depth + 1);
    if ($exploded) {
        if ($leftValue > 0 && $self->{Left}) {
            $self->{Left}->AddRight($leftValue);
        }
        return (1, 0, $rightValue);
    }

    return (0, 0, 0);
}

sub AddLeft {
    my ($self, $value) = @_;
    if ($self->IsRegular()) {
        $self->{Value} += $value;
    } else {
        $self->{Left}->AddLeft($value);
    }
}

sub AddRight {
    my ($self, $value) = @_;
    if ($self->IsRegular()) {
        $self->{Value} += $value;
    } else {
        $self->{Right}->AddRight($value);
    }
}

sub Split {
    my $self = shift;
    if ($self->IsRegular()) {
        if ($self->{Value} >= 10) {
            $self->{Left} = bless { Value => int($self->{Value} / 2) }, 'SnailNumber';
            $self->{Right} = bless { Value => int(($self->{Value} + 1) / 2) }, 'SnailNumber';
            $self->{Value} = -1;
            return 1;
        }
        return 0;
    }
    return $self->{Left}->Split() || $self->{Right}->Split();
}

sub Magnitude {
    my $self = shift;
    if ($self->IsRegular()) {
        return $self->{Value};
    }
    return 3 * $self->{Left}->Magnitude() + 2 * $self->{Right}->Magnitude();
}

package main;

my @snailNumbers;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";

while (my $line = <$fh>) {
    chomp $line;
    push @snailNumbers, parseSnailNumber($line);
}

close $fh;

if (@snailNumbers == 0) {
    print "No snailfish numbers found in the file.\n";
    exit;
}

my $result = $snailNumbers[0];
for (my $i = 1; $i < @snailNumbers; $i++) {
    $result = $result->Add($snailNumbers[$i]);
}

print $result->Magnitude() . "\n";

sub parseSnailNumber {
    my $input = shift;
    $input =~ s/^\s+|\s+$//g;
    if ($input !~ /^\[/) {
        return bless { Value => int($input) }, 'SnailNumber';
    }

    my $balance = 0;
    my $splitIndex = 0;
    for (my $i = 1; $i < length($input) - 1; $i++) {
        my $char = substr($input, $i, 1);
        if ($char eq '[') {
            $balance++;
        } elsif ($char eq ']') {
            $balance--;
        } elsif ($char eq ',') {
            if ($balance == 0) {
                $splitIndex = $i + 1;
                last;
            }
        }
        last if $splitIndex != 0;
    }

    my $left = parseSnailNumber(substr($input, 1, $splitIndex - 2));
    my $right = parseSnailNumber(substr($input, $splitIndex, length($input) - $splitIndex - 1));
    return bless { Left => $left, Right => $right }, 'SnailNumber';
}