
#!/usr/bin/perl

use strict;
use warnings;

# Intcode interpreter
sub run_intcode {
    my ($program, $input_ref) = @_;
    my @memory = split /,/, $program;
    my $ip = 0;
    my $relative_base = 0;
    my $input_index = 0;
    my @output;

    my $get_value = sub {
        my ($mode, $val) = @_;
        if ($mode == 0) {
            return $memory[$val] || 0;
        } elsif ($mode == 1) {
            return $val;
        } elsif ($mode == 2) {
            return $memory[$relative_base + $val] || 0;
        }
    };

    my $set_value = sub {
        my ($mode, $pos, $val) = @_;
        if ($mode == 0) {
            $memory[$pos] = $val;
        } elsif ($mode == 2) {
            $memory[$relative_base + $pos] = $val;
        }
    };

    while ($ip < @memory) {
        my $opcode = $memory[$ip] % 100;
        my $mode1 = int($memory[$ip] / 100) % 10;
        my $mode2 = int($memory[$ip] / 1000) % 10;
        my $mode3 = int($memory[$ip] / 10000) % 10;

        if ($opcode == 1) {
            my $val1 = $get_value->($mode1, $memory[$ip + 1]);
            my $val2 = $get_value->($mode2, $memory[$ip + 2]);
            $set_value->($mode3, $memory[$ip + 3], $val1 + $val2);
            $ip += 4;
        } elsif ($opcode == 2) {
            my $val1 = $get_value->($mode1, $memory[$ip + 1]);
            my $val2 = $get_value->($mode2, $memory[$ip + 2]);
            $set_value->($mode3, $memory[$ip + 3], $val1 * $val2);
            $ip += 4;
        } elsif ($opcode == 3) {
            if ($input_index >= scalar(@{$input_ref})) {
                return (\@output, "INPUT_WAIT");
            }
            $set_value->($mode1, $memory[$ip + 1], $$input_ref[$input_index]);
            $input_index++;
            $ip += 2;
        } elsif ($opcode == 4) {
            my $val1 = $get_value->($mode1, $memory[$ip + 1]);
            push @output, $val1;
            $ip += 2;
        } elsif ($opcode == 5) {
            my $val1 = $get_value->($mode1, $memory[$ip + 1]);
            my $val2 = $get_value->($mode2, $memory[$ip + 2]);
            if ($val1 != 0) {
                $ip = $val2;
            } else {
                $ip += 3;
            }
        } elsif ($opcode == 6) {
            my $val1 = $get_value->($mode1, $memory[$ip + 1]);
            my $val2 = $get_value->($mode2, $memory[$ip + 2]);
            if ($val1 == 0) {
                $ip = $val2;
            } else {
                $ip += 3;
            }
        } elsif ($opcode == 7) {
            my $val1 = $get_value->($mode1, $memory[$ip + 1]);
            my $val2 = $get_value->($mode2, $memory[$ip + 2]);
            $set_value->($mode3, $memory[$ip + 3], ($val1 < $val2) ? 1 : 0);
            $ip += 4;
        } elsif ($opcode == 8) {
            my $val1 = $get_value->($mode1, $memory[$ip + 1]);
            my $val2 = $get_value->($mode2, $memory[$ip + 2]);
            $set_value->($mode3, $memory[$ip + 3], ($val1 == $val2) ? 1 : 0);
            $ip += 4;
        } elsif ($opcode == 9) {
            my $val1 = $get_value->($mode1, $memory[$ip + 1]);
            $relative_base += $val1;
            $ip += 2;
        } elsif ($opcode == 99) {
            return (\@output, "HALT");
        } else {
            die "Unknown opcode: $opcode at $ip";
        }
    }
}
# Springscript for part 2
my $springscript = <<'END';
NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
NOT E T
NOT T T
OR H T
AND T J
RUN
END

# Read Intcode program from input.txt
open(my $fh, '<', 'input.txt') or die "Can't open input.txt: $!";
my $program = <$fh>;
close($fh);
chomp $program;

# Convert springscript to ASCII codes
my @input = map { ord } split //, $springscript;
push @input, 10 if $springscript !~ /\n$/;

# Run Intcode program with springscript
my ($output, $status) = run_intcode($program, \@input);
if ($status eq "HALT") {
    # Print hull damage
    print $output->[-1], "\n";
}
