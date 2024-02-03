
use strict;
use warnings;

# Read the input from input.txt
open(my $fh, '<', 'input.txt') or die "Error reading input file: $!";
my @operations = <$fh>;
close($fh);

# Initial password
my $password = "abcdefgh";

# Apply the operations to the password
foreach my $op (@operations) {
    $password = applyOperation($op, $password);
}

# Print the result
print "$password\n";

sub applyOperation {
    my ($op, $password) = @_;
    my @fields = split(' ', $op);
    if ($fields[0] eq "swap") {
        if ($fields[1] eq "position") {
            my ($x, $y) = (substr($fields[2], 0, 1), substr($fields[5], 0, 1));
            $password = swapPosition($password, $x, $y);
        }
        else {
            my ($x, $y) = ($fields[2], $fields[5]);
            $password = swapLetter($password, $x, $y);
        }
    }
    elsif ($fields[0] eq "rotate") {
        if ($fields[1] eq "left") {
            my $steps = substr($fields[2], 0, 1);
            $password = rotateLeft($password, $steps);
        }
        elsif ($fields[1] eq "right") {
            my $steps = substr($fields[2], 0, 1);
            $password = rotateRight($password, $steps);
        }
        else {
            my $x = substr($fields[6], 0, 1);
            $password = rotateBasedOnPosition($password, $x);
        }
    }
    elsif ($fields[0] eq "reverse") {
        my ($x, $y) = (substr($fields[2], 0, 1), substr($fields[4], 0, 1));
        $password = reversePositions($password, $x, $y);
    }
    elsif ($fields[0] eq "move") {
        my ($x, $y) = (substr($fields[2], 0, 1), substr($fields[5], 0, 1));
        $password = movePosition($password, $x, $y);
    }
    return $password;
}

sub swapPosition {
    my ($password, $x, $y) = @_;
    my @chars = split('', $password);
    ($chars[$x], $chars[$y]) = ($chars[$y], $chars[$x]);
    return join('', @chars);
}

sub swapLetter {
    my ($password, $x, $y) = @_;
    my @chars = split('', $password);
    foreach my $i (0..$#chars) {
        if ($chars[$i] eq $x) {
            $chars[$i] = $y;
        }
        elsif ($chars[$i] eq $y) {
            $chars[$i] = $x;
        }
    }
    return join('', @chars);
}

sub rotateLeft {
    my ($password, $steps) = @_;
    $steps = $steps % length($password);
    return substr($password, $steps) . substr($password, 0, $steps);
}

sub rotateRight {
    my ($password, $steps) = @_;
    $steps = $steps % length($password);
    return substr($password, length($password)-$steps) . substr($password, 0, length($password)-$steps);
}

sub rotateBasedOnPosition {
    my ($password, $x) = @_;
    my $index = index($password, $x);
    my $steps = 1 + $index;
    $steps++ if $index >= 4;
    return rotateRight($password, $steps);
}

sub reversePositions {
    my ($password, $x, $y) = @_;
    my @chars = split('', $password);
    for (my $i = $x, my $j = $y; $i < $j; $i++, $j--) {
        ($chars[$i], $chars[$j]) = ($chars[$j], $chars[$i]);
    }
    return join('', @chars);
}

sub movePosition {
    my ($password, $x, $y) = @_;
    my @chars = split('', $password);
    my $r = $chars[$x];
    splice(@chars, $x, 1);
    splice(@chars, $y, 0, $r);
    return join('', @chars);
}
