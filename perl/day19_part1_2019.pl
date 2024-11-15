
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

# Read Intcode program from input file
open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my $program = <$fh>;
close $fh;

# Parse the Intcode program
my @memory = split(',', $program);

# Intcode computer subroutine
sub run_intcode {
    my ($x, $y) = @_;
    my @code = @memory;
    my $ip = 0;  # instruction pointer
    my $relative_base = 0;
    my @inputs = ($x, $y);
    my $output;

    while (1) {
        my $instruction = $code[$ip];
        my $opcode = $instruction % 100;
        my $mode1 = ($instruction / 100) % 10;
        my $mode2 = ($instruction / 1000) % 10;
        my $mode3 = ($instruction / 10000) % 10;

        # Get parameter values based on modes
        my $get_param = sub {
            my ($param, $mode) = @_;
            $mode //= 0;
            if ($mode == 0) { return $code[$param] // 0; }
            if ($mode == 1) { return $param; }
            if ($mode == 2) { return $code[$relative_base + $param] // 0; }
        };

        my $set_param = sub {
            my ($param, $mode, $value) = @_;
            $mode //= 0;
            if ($mode == 0) { $code[$param] = $value; }
            if ($mode == 2) { $code[$relative_base + $param] = $value; }
        };

        if ($opcode == 99) { last; }

        if ($opcode == 3) {
            my $input = shift @inputs;
            $set_param->($code[$ip+1], $mode1, $input);
            $ip += 2;
        }
        elsif ($opcode == 4) {
            $output = $get_param->($code[$ip+1], $mode1);
            $ip += 2;
            return $output;
        }
        elsif ($opcode == 1) {
            my $val = $get_param->($code[$ip+1], $mode1) + $get_param->($code[$ip+2], $mode2);
            $set_param->($code[$ip+3], $mode3, $val);
            $ip += 4;
        }
        elsif ($opcode == 2) {
            my $val = $get_param->($code[$ip+1], $mode1) * $get_param->($code[$ip+2], $mode2);
            $set_param->($code[$ip+3], $mode3, $val);
            $ip += 4;
        }
        elsif ($opcode == 5) {
            $ip = $get_param->($code[$ip+1], $mode1) != 0 
                ? $get_param->($code[$ip+2], $mode2) 
                : $ip + 3;
        }
        elsif ($opcode == 6) {
            $ip = $get_param->($code[$ip+1], $mode1) == 0 
                ? $get_param->($code[$ip+2], $mode2) 
                : $ip + 3;
        }
        elsif ($opcode == 7) {
            my $val = $get_param->($code[$ip+1], $mode1) < $get_param->($code[$ip+2], $mode2) ? 1 : 0;
            $set_param->($code[$ip+3], $mode3, $val);
            $ip += 4;
        }
        elsif ($opcode == 8) {
            my $val = $get_param->($code[$ip+1], $mode1) == $get_param->($code[$ip+2], $mode2) ? 1 : 0;
            $set_param->($code[$ip+3], $mode3, $val);
            $ip += 4;
        }
        elsif ($opcode == 9) {
            $relative_base += $get_param->($code[$ip+1], $mode1);
            $ip += 2;
        }
    }
    return $output;
}

# Count points affected by tractor beam
my $affected_points = 0;
for my $y (0..49) {
    for my $x (0..49) {
        my $result = run_intcode($x, $y);
        $affected_points++ if $result == 1;
    }
}

print "Points affected by tractor beam: $affected_points\n";
