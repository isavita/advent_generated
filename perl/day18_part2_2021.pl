use strict;
use warnings;

package SnailNumber;

sub new {
    my ($class, $value, $left, $right) = @_;
    my $self = {
        value => $value,
        left  => $left,
        right => $right,
    };
    bless $self, $class;
    return $self;
}

sub is_regular {
    my ($self) = @_;
    return !defined($self->{left}) && !defined($self->{right});
}

sub add {
    my ($self, $other) = @_;
    my $new_number = SnailNumber->new(undef, $self, $other);
    return $new_number->reduce();
}

sub reduce {
    my ($self) = @_;
    while (1) {
        my ($exploded, $left_value, $right_value) = $self->explode(0);
        if ($exploded) {
            next;
        }
        if (!$self->split()) {
            last;
        }
    }
    return $self;
}

sub explode {
    my ($self, $depth) = @_;
    if ($self->is_regular()) {
        return (0, 0, 0);
    }
    if ($depth == 4) {
        my $left_value = $self->{left}{value};
        my $right_value = $self->{right}{value};
        $self->{left} = undef;
        $self->{right} = undef;
        $self->{value} = 0;
        return (1, $left_value, $right_value);
    }
    my ($exploded, $left_value, $right_value) = $self->{left}->explode($depth + 1);
    if ($exploded) {
        if ($right_value > 0 && defined($self->{right})) {
            $self->{right}->add_left($right_value);
        }
        return (1, $left_value, 0);
    }
    ($exploded, $left_value, $right_value) = $self->{right}->explode($depth + 1);
    if ($exploded) {
        if ($left_value > 0 && defined($self->{left})) {
            $self->{left}->add_right($left_value);
        }
        return (1, 0, $right_value);
    }
    return (0, 0, 0);
}

sub add_left {
    my ($self, $value) = @_;
    if ($self->is_regular()) {
        $self->{value} += $value;
    } else {
        $self->{left}->add_left($value);
    }
}

sub add_right {
    my ($self, $value) = @_;
    if ($self->is_regular()) {
        $self->{value} += $value;
    } else {
        $self->{right}->add_right($value);
    }
}

sub split {
    my ($self) = @_;
    if ($self->is_regular()) {
        if ($self->{value} >= 10) {
            $self->{left} = SnailNumber->new(int($self->{value} / 2));
            $self->{right} = SnailNumber->new(int(($self->{value} + 1) / 2));
            $self->{value} = undef;
            return 1;
        }
        return 0;
    }
    return $self->{left}->split() || $self->{right}->split();
}

sub magnitude {
    my ($self) = @_;
    if ($self->is_regular()) {
        return $self->{value};
    }
    return 3 * $self->{left}->magnitude() + 2 * $self->{right}->magnitude();
}

sub deep_copy {
    my ($self) = @_;
    if ($self->is_regular()) {
        return SnailNumber->new($self->{value});
    }
    return SnailNumber->new(undef, $self->{left}->deep_copy(), $self->{right}->deep_copy());
}

package main;

sub parse_snail_number {
    my ($input) = @_;
    $input =~ s/^\s+|\s+$//g;
    if ($input !~ /^\[/) {
        return SnailNumber->new($input);
    }
    my $balance = 0;
    my $split_index = 0;
    for my $i (1 .. length($input) - 2) {
        my $char = substr($input, $i, 1);
        if ($char eq '[') {
            $balance++;
        } elsif ($char eq ']') {
            $balance--;
        } elsif ($char eq ',' && $balance == 0) {
            $split_index = $i;
            last;
        }
    }
    my $left = parse_snail_number(substr($input, 1, $split_index - 1));
    my $right = parse_snail_number(substr($input, $split_index + 1, length($input) - $split_index - 2));
    return SnailNumber->new(undef, $left, $right);
}

open my $file, '<', 'input.txt' or die "Error opening input file: $!";
my @snail_numbers;
while (my $line = <$file>) {
    chomp $line;
    push @snail_numbers, parse_snail_number($line);
}
close $file;

if (@snail_numbers == 0) {
    print "No snailfish numbers found in the file.\n";
    exit;
}

my $largest_magnitude = 0;

for my $i (0 .. $#snail_numbers) {
    for my $j (0 .. $#snail_numbers) {
        next if $i == $j;
        my $a_copy = $snail_numbers[$i]->deep_copy();
        my $b_copy = $snail_numbers[$j]->deep_copy();
        my $sum1 = $a_copy->add($snail_numbers[$j]->deep_copy())->magnitude();
        my $sum2 = $b_copy->add($snail_numbers[$i]->deep_copy())->magnitude();
        $largest_magnitude = $sum1 if $sum1 > $largest_magnitude;
        $largest_magnitude = $sum2 if $sum2 > $largest_magnitude;
    }
}

print "$largest_magnitude\n";