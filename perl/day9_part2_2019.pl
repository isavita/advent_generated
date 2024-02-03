
use strict;
use warnings;

# Read input from file
open(my $fh, '<', 'input.txt') or die $!;
my $program = <$fh>;
close($fh);

# Convert input to integers and extend memory
my @program = split(',', $program);
my %memory;
for my $i (0..$#program) {
    my $value = int($program[$i]);
    $memory{$i} = $value;
}

# Run the Intcode program in sensor boost mode
print runIntcode(\%memory, 2) . "\n";

sub runIntcode {
    my ($memory, $input) = @_;
    my ($output, $ip, $relativeBase) = (0, 0, 0);

    while (1) {
        my $opcode = $memory->{$ip} % 100;
        my $modes = int($memory->{$ip} / 100);

        my $getParam = sub {
            my ($offset) = @_;
            my $mode = 0;
            if (length($modes) >= $offset) {
                $mode = int(substr($modes, -$offset, 1));
            }

            my $param = $memory->{$ip + $offset};
            if ($mode == 0) {
                return $memory->{$param};
            } elsif ($mode == 1) {
                return $param;
            } elsif ($mode == 2) {
                return $memory->{$relativeBase + $param};
            } else {
                die "unknown parameter mode";
            }
        };

        my $setParam = sub {
            my ($offset, $value) = @_;
            my $mode = 0;
            if (length($modes) >= $offset) {
                $mode = int(substr($modes, -$offset, 1));
            }

            my $param = $memory->{$ip + $offset};
            if ($mode == 0) {
                $memory->{$param} = $value;
            } elsif ($mode == 2) {
                $memory->{$relativeBase + $param} = $value;
            } else {
                die "unknown parameter mode";
            }
        };

        if ($opcode == 1) {
            $setParam->(3, $getParam->(1) + $getParam->(2));
            $ip += 4;
        } elsif ($opcode == 2) {
            $setParam->(3, $getParam->(1) * $getParam->(2));
            $ip += 4;
        } elsif ($opcode == 3) {
            $setParam->(1, $input);
            $ip += 2;
        } elsif ($opcode == 4) {
            $output = $getParam->(1);
            $ip += 2;
        } elsif ($opcode == 5) {
            if ($getParam->(1) != 0) {
                $ip = $getParam->(2);
            } else {
                $ip += 3;
            }
        } elsif ($opcode == 6) {
            if ($getParam->(1) == 0) {
                $ip = $getParam->(2);
            } else {
                $ip += 3;
            }
        } elsif ($opcode == 7) {
            if ($getParam->(1) < $getParam->(2)) {
                $setParam->(3, 1);
            } else {
                $setParam->(3, 0);
            }
            $ip += 4;
        } elsif ($opcode == 8) {
            if ($getParam->(1) == $getParam->(2)) {
                $setParam->(3, 1);
            } else {
                $setParam->(3, 0);
            }
            $ip += 4;
        } elsif ($opcode == 9) {
            $relativeBase += $getParam->(1);
            $ip += 2;
        } elsif ($opcode == 99) {
            return $output;
        } else {
            die "unknown opcode: $opcode";
        }
    }
}
