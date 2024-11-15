
#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;

# Constants
use constant {
    BROADCASTER => 0,
    FLIP_FLOP => 1,
    CONJUNCTION => 2,
};

# Global variables
my %connections;
my %loop_lengths;

sub handle_line {
    my ($line) = @_;
    if ($line =~ /broadcaster/) {
        my ($name, $targets) = split(' -> ', $line);
        $connections{$name} = {
            type => BROADCASTER,
            connects_to => [split(', ', $targets)],
        };
    } elsif ($line =~ /^%(\w+)/) {
        my $name = $1;
        my ($targets) = $line =~ / -> (.+)/;
        $connections{$name} = {
            type => FLIP_FLOP,
            connects_to => [split(', ', $targets)],
            state => 0,
        };
    } elsif ($line =~ /^&(\w+)/) {
        my $name = $1;
        my ($targets) = $line =~ / -> (.+)/;
        $connections{$name} = {
            type => CONJUNCTION,
            connects_to => [split(', ', $targets)],
            watches => {},
        };
    }
}

sub complete_watches {
    for my $name (keys %connections) {
        next unless $connections{$name}{type} == CONJUNCTION;
        
        for my $module_name (keys %connections) {
            my $module = $connections{$module_name};
            for my $target (@{$module->{connects_to}}) {
                if ($target eq $name) {
                    $connections{$name}{watches}{$module_name} = 0;
                }
            }
        }
    }
}

sub simulate_press {
    my ($press_number) = @_;
    my @queue = ({from => 'button', name => 'broadcaster', pulse => 0});
    my @pulses = (1, 0);

    while (@queue) {
        my $curr_state = shift @queue;
        my $module = $connections{$curr_state->{name}};
        
        next unless $module;
        
        my $pulse = $curr_state->{pulse};
        
        if ($curr_state->{name} eq 'rx' && !$pulse) {
            return 1;
        }
        
        if ($module->{type} == BROADCASTER) {
            for my $target (@{$module->{connects_to}}) {
                push @queue, {from => $curr_state->{name}, name => $target, pulse => $pulse};
                $pulses[$pulse]++;
            }
        }
        elsif ($module->{type} == FLIP_FLOP) {
            unless ($pulse) {
                $module->{state} = !$module->{state};
                for my $target (@{$module->{connects_to}}) {
                    push @queue, {from => $curr_state->{name}, name => $target, pulse => $module->{state}};
                    $pulses[$module->{state}]++;
                }
            }
        }
        elsif ($module->{type} == CONJUNCTION) {
            $module->{watches}{$curr_state->{from}} = $pulse;
            
            my $all_true = 1;
            for my $state (values %{$module->{watches}}) {
                $all_true = 0 unless $state;
            }
            
            for my $target (@{$module->{connects_to}}) {
                push @queue, {from => $curr_state->{name}, name => $target, pulse => !$all_true};
                $pulses[!$all_true]++;
            }
            
            if (exists $loop_lengths{$curr_state->{name}} && !$all_true && $loop_lengths{$curr_state->{name}} == -1) {
                $loop_lengths{$curr_state->{name}} = $press_number;
            }
        }
    }
    
    return 0;
}

# Main program
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
while (my $line = <$fh>) {
    chomp $line;
    handle_line($line);
}
close $fh;

complete_watches();

# Find the module connecting to 'rx'
my @px_prev = grep { 
    grep { $_ eq 'rx' } @{$connections{$_}{connects_to}} 
} keys %connections;

die "Error: more than one pxPrev" if @px_prev != 1;

# Initialize loop lengths
for my $name (keys %{$connections{$px_prev[0]}{watches}}) {
    $loop_lengths{$name} = -1;
}

my $press_number = 0;
while (1) {
    $press_number++;
    last if simulate_press($press_number);
    
    my $complete = 1;
    for my $length (values %loop_lengths) {
        if ($length == -1) {
            $complete = 0;
            last;
        }
    }
    last if $complete;
}

my $sum = 1;
$sum *= $_ for values %loop_lengths;

print "$sum\n";
