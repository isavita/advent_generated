
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(max min);

# Read Intcode program from input file
open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my $program = <$fh>;
close $fh;

# Split program into array of integers
my @memory = split(',', $program);

# Initialize robot state
my %panels;
my $x = 0;
my $y = 0;
my $direction = 0; # 0: up, 1: right, 2: down, 3: left

# Directions for movement
my @dx = (0, 1, 0, -1);
my @dy = (-1, 0, 1, 0);

# Intcode computer parameters
my $ip = 0;  # instruction pointer
my $relative_base = 0;
my $halted = 0;

# Input/output queues
my @input_queue;
my @output_queue;

# Extend memory if needed
sub get_memory {
    my ($addr) = @_;
    $memory[$addr] = 0 if !defined $memory[$addr];
    return $memory[$addr];
}

sub set_memory {
    my ($addr, $value) = @_;
    $memory[$addr] = $value;
}

# Parse parameter modes
sub get_param {
    my ($mode, $param, $write) = @_;
    
    if ($write) {
        return $mode == 2 ? $param + $relative_base : $param;
    }
    
    if ($mode == 0) {
        return get_memory($param);
    } elsif ($mode == 1) {
        return $param;
    } elsif ($mode == 2) {
        return get_memory($param + $relative_base);
    }
}

# Intcode computer
sub run_intcode {
    while (!$halted) {
        my $instruction = get_memory($ip);
        my $opcode = $instruction % 100;
        my $mode1 = int(($instruction % 1000) / 100);
        my $mode2 = int(($instruction % 10000) / 1000);
        my $mode3 = int(($instruction % 100000) / 10000);

        if ($opcode == 99) {
            $halted = 1;
            last;
        }

        if ($opcode == 3) {
            if (@input_queue == 0) {
                # Get current panel color
                push @input_queue, $panels{"$x,$y"} // 0;
            }
            
            my $input_addr = get_param($mode1, get_memory($ip + 1), 1);
            set_memory($input_addr, shift @input_queue);
            $ip += 2;
        }
        elsif ($opcode == 4) {
            my $output = get_param($mode1, get_memory($ip + 1), 0);
            push @output_queue, $output;
            $ip += 2;

            # Process outputs when we have two
            if (@output_queue == 2) {
                my $paint_color = shift @output_queue;
                my $turn_direction = shift @output_queue;

                # Paint current panel
                $panels{"$x,$y"} = $paint_color;

                # Turn robot
                $direction = ($direction + ($turn_direction ? 1 : -1) + 4) % 4;

                # Move forward
                $x += $dx[$direction];
                $y += $dy[$direction];
            }
        }
        elsif ($opcode == 5) {
            my $test = get_param($mode1, get_memory($ip + 1), 0);
            $ip = $test ? get_param($mode2, get_memory($ip + 2), 0) : $ip + 3;
        }
        elsif ($opcode == 6) {
            my $test = get_param($mode1, get_memory($ip + 1), 0);
            $ip = $test == 0 ? get_param($mode2, get_memory($ip + 2), 0) : $ip + 3;
        }
        elsif ($opcode == 7) {
            my $param1 = get_param($mode1, get_memory($ip + 1), 0);
            my $param2 = get_param($mode2, get_memory($ip + 2), 0);
            my $dest = get_param($mode3, get_memory($ip + 3), 1);
            set_memory($dest, $param1 < $param2 ? 1 : 0);
            $ip += 4;
        }
        elsif ($opcode == 8) {
            my $param1 = get_param($mode1, get_memory($ip + 1), 0);
            my $param2 = get_param($mode2, get_memory($ip + 2), 0);
            my $dest = get_param($mode3, get_memory($ip + 3), 1);
            set_memory($dest, $param1 == $param2 ? 1 : 0);
            $ip += 4;
        }
        elsif ($opcode == 9) {
            $relative_base += get_param($mode1, get_memory($ip + 1), 0);
            $ip += 2;
        }
        else {
            my $param1 = get_param($mode1, get_memory($ip + 1), 0);
            my $param2 = get_param($mode2, get_memory($ip + 2), 0);
            my $dest = get_param($mode3, get_memory($ip + 3), 1);
            
            if ($opcode == 1) {
                set_memory($dest, $param1 + $param2);
            }
            elsif ($opcode == 2) {
                set_memory($dest, $param1 * $param2);
            }
            
            $ip += 4;
        }
    }
}

# Run the robot
run_intcode();

# Count unique panels painted
my $unique_panels = scalar(keys %panels);
print "Panels painted at least once: $unique_panels\n";
