
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(max);

# Intcode computer
sub run_intcode {
    my ($program, $inputs) = @_;
    my @memory = @$program;
    my $ip = 0;
    my $input_index = 0;
    my @outputs;

    while ($ip < @memory) {
        my $opcode = $memory[$ip] % 100;
        my $mode1 = int($memory[$ip] / 100) % 10;
        my $mode2 = int($memory[$ip] / 1000) % 10;

        if ($opcode == 1) {
            my $val1 = $mode1 ? $memory[$ip + 1] : $memory[$memory[$ip + 1]];
            my $val2 = $mode2 ? $memory[$ip + 2] : $memory[$memory[$ip + 2]];
            $memory[$memory[$ip + 3]] = $val1 + $val2;
            $ip += 4;
        } elsif ($opcode == 2) {
            my $val1 = $mode1 ? $memory[$ip + 1] : $memory[$memory[$ip + 1]];
            my $val2 = $mode2 ? $memory[$ip + 2] : $memory[$memory[$ip + 2]];
            $memory[$memory[$ip + 3]] = $val1 * $val2;
            $ip += 4;
        } elsif ($opcode == 3) {
            $memory[$memory[$ip + 1]] = $inputs->[$input_index++];
            $ip += 2;
        } elsif ($opcode == 4) {
            my $val1 = $mode1 ? $memory[$ip + 1] : $memory[$memory[$ip + 1]];
            push @outputs, $val1;
            $ip += 2;
        } elsif ($opcode == 5) {
            my $val1 = $mode1 ? $memory[$ip + 1] : $memory[$memory[$ip + 1]];
            my $val2 = $mode2 ? $memory[$ip + 2] : $memory[$memory[$ip + 2]];
            $ip = ($val1 != 0) ? $val2 : $ip + 3;
        } elsif ($opcode == 6) {
            my $val1 = $mode1 ? $memory[$ip + 1] : $memory[$memory[$ip + 1]];
            my $val2 = $mode2 ? $memory[$ip + 2] : $memory[$memory[$ip + 2]];
            $ip = ($val1 == 0) ? $val2 : $ip + 3;
        } elsif ($opcode == 7) {
            my $val1 = $mode1 ? $memory[$ip + 1] : $memory[$memory[$ip + 1]];
            my $val2 = $mode2 ? $memory[$ip + 2] : $memory[$memory[$ip + 2]];
            $memory[$memory[$ip + 3]] = ($val1 < $val2) ? 1 : 0;
            $ip += 4;
        } elsif ($opcode == 8) {
            my $val1 = $mode1 ? $memory[$ip + 1] : $memory[$memory[$ip + 1]];
            my $val2 = $mode2 ? $memory[$ip + 2] : $memory[$memory[$ip + 2]];
            $memory[$memory[$ip + 3]] = ($val1 == $val2) ? 1 : 0;
            $ip += 4;
        } elsif ($opcode == 99) {
            last;
        } else {
            die "Invalid opcode: $opcode";
        }
    }
    return \@outputs;
}

# Generate permutations
sub permutations {
    my ($arr) = @_;
    return [[]] if @$arr == 0;
    my @result;
    for my $i (0 .. $#$arr) {
        my @rest = @$arr;
        my $elem = splice @rest, $i, 1;
        for my $perm (@{permutations(\@rest)}) {
            push @result, [$elem, @$perm];
        }
    }
    return \@result;
}

# Read program from input file
open(my $fh, '<', 'input.txt') or die "Could not open file: $!";
my $line = <$fh>;
close($fh);
chomp $line;
my @program = split ',', $line;

# Part 1
my $max_output = 0;
for my $perm (@{permutations([0, 1, 2, 3, 4])}) {
    my $input = 0;
    for my $phase (@$perm) {
        my $output = run_intcode(\@program, [$phase, $input]);
        $input = $output->[0];
    }
    $max_output = max($max_output, $input);
}
print "Part 1: $max_output\n";

# Part 2
$max_output = 0;
for my $perm (@{permutations([5, 6, 7, 8, 9])}) {
    my @amps;
    for (1..5) {
        push @amps, { program => [@program], ip => 0, inputs => [], input_index => 0 };
    }
    my $input = 0;
    my $amp_index = 0;
    my $first_round = 1;
    
    while (1) {
        my $amp = $amps[$amp_index];
        if ($first_round) {
            push @{$amp->{inputs}}, $perm->[$amp_index];
        }
        push @{$amp->{inputs}}, $input;

        my $opcode;
        my @outputs;

        while ($amp->{ip} < @{$amp->{program}}) {
            $opcode = $amp->{program}->[$amp->{ip}] % 100;
            my $mode1 = int($amp->{program}->[$amp->{ip}] / 100) % 10;
            my $mode2 = int($amp->{program}->[$amp->{ip}] / 1000) % 10;

            if ($opcode == 1) {
                my $val1 = $mode1 ? $amp->{program}->[$amp->{ip} + 1] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 1]];
                my $val2 = $mode2 ? $amp->{program}->[$amp->{ip} + 2] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 2]];
                $amp->{program}->[$amp->{program}->[$amp->{ip} + 3]] = $val1 + $val2;
                $amp->{ip} += 4;
            } elsif ($opcode == 2) {
                my $val1 = $mode1 ? $amp->{program}->[$amp->{ip} + 1] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 1]];
                my $val2 = $mode2 ? $amp->{program}->[$amp->{ip} + 2] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 2]];
                $amp->{program}->[$amp->{program}->[$amp->{ip} + 3]] = $val1 * $val2;
                $amp->{ip} += 4;
            } elsif ($opcode == 3) {
                if ($amp->{input_index} < @{$amp->{inputs}}) {
                  $amp->{program}->[$amp->{program}->[$amp->{ip} + 1]] = $amp->{inputs}->[$amp->{input_index}++];
                  $amp->{ip} += 2;
                } else {
                  last;
                }
            } elsif ($opcode == 4) {
                my $val1 = $mode1 ? $amp->{program}->[$amp->{ip} + 1] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 1]];
                push @outputs, $val1;
                $amp->{ip} += 2;
            } elsif ($opcode == 5) {
                my $val1 = $mode1 ? $amp->{program}->[$amp->{ip} + 1] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 1]];
                my $val2 = $mode2 ? $amp->{program}->[$amp->{ip} + 2] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 2]];
                $amp->{ip} = ($val1 != 0) ? $val2 : $amp->{ip} + 3;
            } elsif ($opcode == 6) {
                my $val1 = $mode1 ? $amp->{program}->[$amp->{ip} + 1] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 1]];
                my $val2 = $mode2 ? $amp->{program}->[$amp->{ip} + 2] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 2]];
                $amp->{ip} = ($val1 == 0) ? $val2 : $amp->{ip} + 3;
            } elsif ($opcode == 7) {
                my $val1 = $mode1 ? $amp->{program}->[$amp->{ip} + 1] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 1]];
                my $val2 = $mode2 ? $amp->{program}->[$amp->{ip} + 2] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 2]];
                $amp->{program}->[$amp->{program}->[$amp->{ip} + 3]] = ($val1 < $val2) ? 1 : 0;
                $amp->{ip} += 4;
            } elsif ($opcode == 8) {
                my $val1 = $mode1 ? $amp->{program}->[$amp->{ip} + 1] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 1]];
                my $val2 = $mode2 ? $amp->{program}->[$amp->{ip} + 2] : $amp->{program}->[$amp->{program}->[$amp->{ip} + 2]];
                $amp->{program}->[$amp->{program}->[$amp->{ip} + 3]] = ($val1 == $val2) ? 1 : 0;
                $amp->{ip} += 4;
            } elsif ($opcode == 99) {
                last;
            } else {
                die "Invalid opcode: $opcode";
            }
        }

        if (@outputs) {
          $input = shift @outputs;
        }
        
        if ($opcode == 99 && $amp_index == 4) {
            last;
        }

        $amp_index = ($amp_index + 1) % 5;
        if ($amp_index == 0) {
            $first_round = 0;
        }
    }

    $max_output = max($max_output, $input);
}
print "Part 2: $max_output\n";
