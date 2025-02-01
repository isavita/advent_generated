#!/usr/bin/perl
use strict;
use warnings;
use Storable 'dclone';

# Read the Intcode program from input.txt and store it as an array.
open(my $fh, '<', "input.txt") or die "Could not open input.txt: $!";
chomp(my $line = <$fh>);
close($fh);
my @program = split(/,/, $line);

# Global variables to store the discovered map.
# %grid: key "x,y" -> value '#' for wall, '.' for free space, or 'O' for oxygen system.
my %grid;
# A hash to mark visited coordinates during exploration.
my %visited;

# Mapping for movement commands and coordinates.
# Commands: 1 north, 2 south, 3 west, 4 east.
my %move = (
    1 => { dx => 0,  dy => -1 },
    2 => { dx => 0,  dy => 1 },
    3 => { dx => -1, dy => 0 },
    4 => { dx => 1,  dy => 0 },
);
# Reverse directions: north <-> south, east <-> west.
my %reverse = (
    1 => 2,
    2 => 1,
    3 => 4,
    4 => 3,
);

# Build the initial Intcode machine state.
# Our machine state is a hash with keys:
#   mem          => array ref holding the program (and its memory).
#   ip           => instruction pointer.
#   relative_base=> relative base for addressing.
sub make_machine {
    my @mem = @program;  # copy of the program
    return { mem => \@mem, ip => 0, relative_base => 0 };
}

# Helper: get value at memory address idx, treating undefined as 0.
sub get_memory {
    my ($machine, $idx) = @_;
    return 0 if $idx < 0;
    return $machine->{mem}->[$idx] // 0;
}

# Helper: set value at memory address idx.
sub set_memory {
    my ($machine, $idx, $val) = @_;
    $machine->{mem}->[$idx] = $val;
}

# Run the Intcode machine until an output is produced.
# Supply a single integer input ($in_val) to the machine.
# Returns the output value when an output instruction is executed.
sub run_machine {
    my ($machine, $in_val) = @_;
    # A flag so that we only use the provided input once.
    my $input_used = 0;
    while (1) {
        my $instr = get_memory($machine, $machine->{ip});
        my $opcode = $instr % 100;
        my $modes = int($instr / 100);
        
        # Helper to get mode for parameter n (starting at 1).
        my sub get_mode {
            my ($n) = @_;
            return int($modes / (10 ** ($n - 1))) % 10;
        }
        
        # Helper for reading a parameter value.
        my sub read_param {
            my ($n) = @_;
            my $mode = get_mode($n);
            my $param = get_memory($machine, $machine->{ip} + $n);
            if ($mode == 0) {         # position mode
                return get_memory($machine, $param);
            } elsif ($mode == 1) {    # immediate mode
                return $param;
            } elsif ($mode == 2) {    # relative mode
                return get_memory($machine, $param + $machine->{relative_base});
            }
            die "Unknown mode: $mode";
        }
        
        # Helper for writing a value to a parameter.
        my sub write_param {
            my ($n, $value) = @_;
            my $mode = get_mode($n);
            my $param = get_memory($machine, $machine->{ip} + $n);
            my $addr;
            if ($mode == 0) {
                $addr = $param;
            } elsif ($mode == 2) {
                $addr = $param + $machine->{relative_base};
            } else {
                die "Invalid mode for write: $mode";
            }
            set_memory($machine, $addr, $value);
        }
        
        if ($opcode == 1) {
            # Addition
            my $a = read_param(1);
            my $b = read_param(2);
            write_param(3, $a + $b);
            $machine->{ip} += 4;
        }
        elsif ($opcode == 2) {
            # Multiplication
            my $a = read_param(1);
            my $b = read_param(2);
            write_param(3, $a * $b);
            $machine->{ip} += 4;
        }
        elsif ($opcode == 3) {
            # Input
            if ($input_used) {
                die "No more input available!";
            }
            write_param(1, $in_val);
            $input_used = 1;
            $machine->{ip} += 2;
        }
        elsif ($opcode == 4) {
            # Output: return this value.
            my $out_val = read_param(1);
            $machine->{ip} += 2;
            return $out_val;
        }
        elsif ($opcode == 5) {
            # Jump-if-true
            my $a = read_param(1);
            my $b = read_param(2);
            if ($a != 0) {
                $machine->{ip} = $b;
            } else {
                $machine->{ip} += 3;
            }
        }
        elsif ($opcode == 6) {
            # Jump-if-false
            my $a = read_param(1);
            my $b = read_param(2);
            if ($a == 0) {
                $machine->{ip} = $b;
            } else {
                $machine->{ip} += 3;
            }
        }
        elsif ($opcode == 7) {
            # Less than
            my $a = read_param(1);
            my $b = read_param(2);
            write_param(3, ($a < $b) ? 1 : 0);
            $machine->{ip} += 4;
        }
        elsif ($opcode == 8) {
            # Equals
            my $a = read_param(1);
            my $b = read_param(2);
            write_param(3, ($a == $b) ? 1 : 0);
            $machine->{ip} += 4;
        }
        elsif ($opcode == 9) {
            # Adjust relative base
            my $a = read_param(1);
            $machine->{relative_base} += $a;
            $machine->{ip} += 2;
        }
        elsif ($opcode == 99) {
            die "Machine halted unexpectedly";
        }
        else {
            die "Unknown opcode: $opcode at position $machine->{ip}";
        }
    }
}

# DFS exploration of the maze.
# Arguments:
#   $x, $y: current coordinates.
#   $machine: pointer to the current intcode machine state.
sub dfs_explore {
    my ($x, $y, $machine) = @_;
    my $pos_key = "$x,$y";
    $visited{$pos_key} = 1;
    # Mark the current position as open (or oxygen if discovered previously).
    $grid{$pos_key} //= '.';
    
    foreach my $dir (1,2,3,4) {
        my $dx = $move{$dir}->{dx};
        my $dy = $move{$dir}->{dy};
        my $nx = $x + $dx;
        my $ny = $y + $dy;
        my $npos = "$nx,$ny";
        # Avoid revisiting positions.
        next if exists $visited{$npos};
        
        # Clone the machine state so that we can try moving in this direction.
        my $machine_clone = dclone($machine);
        my $status = run_machine($machine_clone, $dir);
        if ($status == 0) {
            # Hit a wall; mark the position as wall.
            $grid{$npos} = '#';
        }
        elsif ($status == 1 || $status == 2) {
            # Successful movement: mark as open; if 2, mark oxygen system.
            $grid{$npos} = ($status == 2) ? 'O' : '.';
            dfs_explore($nx, $ny, $machine_clone);
            # Backtrack: send the reverse command to return to the previous position.
            my $rev = $reverse{$dir};
            my $back_status = run_machine($machine_clone, $rev);
            die "Backtracking failed at ($nx,$ny) moving $rev" unless $back_status == 1;
        }
    }
}

# After exploring, perform a BFS on the discovered grid to find the shortest path
# from the starting position (0,0) to the oxygen system ('O').
sub bfs_shortest_path {
    my $start = "0,0";
    my %bvisited = ( $start => 1 );
    my @queue = ( { pos => $start, dist => 0 } );
    
    while (@queue) {
        my $current = shift @queue;
        my ($cx, $cy) = split /,/, $current->{pos};
        # Check if we have found the oxygen system.
        if ($grid{$current->{pos}} eq 'O') {
            return $current->{dist};
        }
        # Explore neighbors.
        foreach my $d (values %move) {
            my $nx = $cx + $d->{dx};
            my $ny = $cy + $d->{dy};
            my $npos = "$nx,$ny";
            next if exists $bvisited{$npos};
            # Only traverse positions that are not walls.
            next unless defined $grid{$npos} and $grid{$npos} ne '#';
            $bvisited{$npos} = 1;
            push @queue, { pos => $npos, dist => $current->{dist} + 1 };
        }
    }
    return -1;  # not found
}

# Main execution:
my $machine = make_machine();
dfs_explore(0, 0, $machine);
my $steps = bfs_shortest_path();
if ($steps >= 0) {
    print "$steps\n";
} else {
    print "Oxygen system not found.\n";
}

# End of program.
  
__END__

=pod

This Perl program reads an Intcode program from input.txt, uses a DFS-based exploration to map out
the area by simulating the repair droid (via an Intcode machine), and then performs a BFS over the
discovered grid to compute the fewest movement commands needed from the starting position to reach
the oxygen system. The result is output on standard output.

=cut