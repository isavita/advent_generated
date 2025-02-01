#!/usr/bin/perl
use strict;
use warnings;
use feature 'say';

# Read the comma‐separated Intcode program from input.txt
open(my $fh, "<", "input.txt") or die "Can't open input.txt: $!";
chomp(my $line = <$fh>);
close $fh;
my @program = split /,/, $line;

# We'll simulate 50 networked Intcode computers.
my $NUM = 50;
my @computers;
for my $i (0 .. $NUM-1) {
    # Each computer gets its own copy of the program,
    # with its instruction pointer (ip), relative base (rb),
    # an input queue and an output buffer.
    $computers[$i] = {
        mem    => [ @program ],    # copy of the program memory
        ip     => 0,               # instruction pointer
        rb     => 0,               # relative base for addressing
        input  => [ $i ],          # first input is its network address
        output => [],              # output buffer (will collect triple outputs)
        idle   => 0,               # flag whether last input was -1 (idle)
        halted => 0,               # set if program halts (should not happen in this puzzle)
    };
}

# NAT (Not Always Transmitting) stores the last packet sent to address 255.
my $nat_packet   = undef;
my $prev_nat_y   = undef;

# Main simulation loop.
while (1) {
    my $network_activity = 0;  # whether any packet was delivered this cycle

    # Process one instruction (one step) for each computer.
    for my $comp (@computers) {
        next if $comp->{halted};

        run_step($comp);

        # Check if output buffer holds a complete packet (three values).
        while ( @{$comp->{output}} >= 3 ) {
            my $dest = shift @{$comp->{output}};
            my $X    = shift @{$comp->{output}};
            my $Y    = shift @{$comp->{output}};
            $network_activity = 1;

            if ($dest == 255) {
                # NAT packet: save/overwrite the stored packet.
                $nat_packet = { x => $X, y => $Y };
            }
            elsif ($dest >= 0 and $dest < $NUM) {
                # Send X, Y to the indicated computer.
                push @{ $computers[$dest]->{input} }, $X, $Y;
                $computers[$dest]->{idle} = 0;
            }
        }
    }

    # Check if every computer is idle.
    my $all_idle = 1;
    for my $comp (@computers) {
        # If the computer has pending input or is not marked idle, then it is active.
        if ( @{ $comp->{input} } or ! $comp->{idle} ) {
            $all_idle = 0;
            last;
        }
    }

    # If the network is idle then the NAT sends its stored packet to computer 0.
    if ($all_idle) {
        if (defined $nat_packet) {
            push @{ $computers[0]->{input} }, $nat_packet->{x}, $nat_packet->{y};
            $computers[0]->{idle} = 0;
            # If the same Y value is delivered twice in a row then output it and exit.
            if (defined $prev_nat_y and $prev_nat_y == $nat_packet->{y}) {
                say $nat_packet->{y};
                exit 0;
            }
            $prev_nat_y = $nat_packet->{y};
        }
    }
}

#
# Subroutine: run_step
# Executes a single Intcode instruction for a computer.
#
sub run_step {
    my ($comp) = @_;
    my $ip  = $comp->{ip};
    my $mem = $comp->{mem};

    my $instr = get_val($mem, $ip);
    my $op    = $instr % 100;
    my $mode1 = int($instr/100)    % 10;
    my $mode2 = int($instr/1000)   % 10;
    my $mode3 = int($instr/10000)  % 10;

    if ($op == 1) {         # Addition
        my $a = get_param($comp, $ip+1, $mode1);
        my $b = get_param($comp, $ip+2, $mode2);
        set_param($comp, $ip+3, $mode3, $a + $b);
        $comp->{ip} += 4;
    }
    elsif ($op == 2) {      # Multiplication
        my $a = get_param($comp, $ip+1, $mode1);
        my $b = get_param($comp, $ip+2, $mode2);
        set_param($comp, $ip+3, $mode3, $a * $b);
        $comp->{ip} += 4;
    }
    elsif ($op == 3) {      # Input
        my $input_val;
        if (@{ $comp->{input} }) {
            $input_val = shift @{ $comp->{input} };
            $comp->{idle} = 0;  # receiving real input, so not idle
        }
        else {
            $input_val = -1;
            $comp->{idle} = 1;
        }
        set_param($comp, $ip+1, $mode1, $input_val);
        $comp->{ip} += 2;
    }
    elsif ($op == 4) {      # Output
        my $val = get_param($comp, $ip+1, $mode1);
        push @{ $comp->{output} }, $val;
        $comp->{idle} = 0;  # producing output means activity
        $comp->{ip} += 2;
    }
    elsif ($op == 5) {      # Jump-if-true
        my $a = get_param($comp, $ip+1, $mode1);
        my $b = get_param($comp, $ip+2, $mode2);
        if ($a != 0) { $comp->{ip} = $b; }
        else         { $comp->{ip} += 3; }
    }
    elsif ($op == 6) {      # Jump-if-false
        my $a = get_param($comp, $ip+1, $mode1);
        my $b = get_param($comp, $ip+2, $mode2);
        if ($a == 0) { $comp->{ip} = $b; }
        else         { $comp->{ip} += 3; }
    }
    elsif ($op == 7) {      # Less than
        my $a = get_param($comp, $ip+1, $mode1);
        my $b = get_param($comp, $ip+2, $mode2);
        set_param($comp, $ip+3, $mode3, ($a < $b) ? 1 : 0);
        $comp->{ip} += 4;
    }
    elsif ($op == 8) {      # Equals
        my $a = get_param($comp, $ip+1, $mode1);
        my $b = get_param($comp, $ip+2, $mode2);
        set_param($comp, $ip+3, $mode3, ($a == $b) ? 1 : 0);
        $comp->{ip} += 4;
    }
    elsif ($op == 9) {      # Adjust relative base
        my $a = get_param($comp, $ip+1, $mode1);
        $comp->{rb} += $a;
        $comp->{ip} += 2;
    }
    elsif ($op == 99) {     # Halt
        $comp->{halted} = 1;
        $comp->{ip} += 1;
    }
    else {
        die "Unknown opcode $op at ip $ip\n";
    }
}

#
# Helper: get_val
# Returns value from memory at index $idx (defaulting to 0 if not set).
#
sub get_val {
    my ($mem, $idx) = @_;
    return defined $mem->[$idx] ? $mem->[$idx] : 0;
}

#
# Helper: get_param
# Interprets a parameter value according to its mode.
#
sub get_param {
    my ($comp, $idx, $mode) = @_;
    my $mem = $comp->{mem};
    if ($mode == 0) {
        return get_val($mem, get_val($mem, $idx));
    }
    elsif ($mode == 1) {
        return get_val($mem, $idx);
    }
    elsif ($mode == 2) {
        return get_val($mem, get_val($mem, $idx) + $comp->{rb});
    }
    else {
        die "Unknown parameter mode $mode\n";
    }
}

#
# Helper: set_param
# Writes a value to memory using the appropriate addressing mode.
#
sub set_param {
    my ($comp, $idx, $mode, $value) = @_;
    my $mem = $comp->{mem};
    my $addr;
    if ($mode == 0) {
        $addr = get_val($mem, $idx);
    }
    elsif ($mode == 2) {
        $addr = get_val($mem, $idx) + $comp->{rb};
    }
    else {
        die "Invalid mode for write parameter: $mode\n";
    }
    $mem->[$addr] = $value;
}

# End of program.
 
__END__

=pod

This Perl program reads the Intcode software from "input.txt" and
simulates a network of 50 NICs communicating with each other. Packets
are sent as groups of three outputs (destination, X, Y). A special NAT
device stores packets sent to address 255. When all computers are idle,
the NAT sends its stored packet to computer 0. The program terminates
when the NAT sends a Y value to computer 0 that is the same as the last
Y value it sent; at that point the repeated Y value is printed.

To run the solution:
  • Place your puzzle input (Intcode software) in "input.txt".
  • Run this script with: perl this_script.pl

=cut