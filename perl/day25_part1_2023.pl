
#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;

sub parse_input {
    my ($input) = @_;
    my %graph;
    
    for my $line (@$input) {
        my ($vertice, $others) = split(/: /, $line);
        my @connected = split(/ /, $others);
        
        $graph{$vertice} //= {};
        $graph{$_} //= {} for @connected;
        
        for my $other (@connected) {
            $graph{$vertice}{$other} = 1;
            $graph{$other}{$vertice} = 1;
        }
    }
    
    return \%graph;
}

sub breadth_first_search {
    my ($graph, $start, $goal_func) = @_;
    my @frontier = ($start);
    my %reached = ($start => 1);
    my %came_from = ($start => $start);
    
    while (@frontier) {
        my $current = shift @frontier;
        
        return (\%came_from, 1) if $goal_func->($current);
        
        for my $next (keys %{$graph->{$current}}) {
            unless ($reached{$next}) {
                push @frontier, $next;
                $reached{$next} = 1;
                $came_from{$next} = $current;
            }
        }
    }
    
    return (\%came_from, 0);
}

sub reconstruct_path {
    my ($start, $end, $came_from) = @_;
    my @path;
    my $current = $end;
    
    while ($current ne $start) {
        unshift @path, $current;
        $current = $came_from->{$current};
    }
    
    unshift @path, $start;
    return \@path;
}

sub copy_graph {
    my ($graph) = @_;
    my %new_graph;
    
    for my $vertice (keys %$graph) {
        $new_graph{$vertice} = { %{$graph->{$vertice}} };
    }
    
    return \%new_graph;
}

sub solve {
    my ($input) = @_;
    my $min_cut = 3;
    my $graph = parse_input($input);
    
    my ($source) = keys %$graph;
    
    for my $end (keys %$graph) {
        next if $end eq $source;
        
        my $new_graph = copy_graph($graph);
        
        for (1..$min_cut) {
            my ($came_from, $found) = breadth_first_search($new_graph, $source, sub { $_[0] eq $end });
            last unless $found;
            
            my $path = reconstruct_path($source, $end, $came_from);
            
            for my $i (0..$#{$path}-1) {
                delete $new_graph->{$path->[$i]}{$path->[$i+1]};
                delete $new_graph->{$path->[$i+1]}{$path->[$i]};
            }
        }
        
        my ($came_from, $found) = breadth_first_search($new_graph, $source, sub { 0 });
        
        if (!$found) {
            my $length1 = scalar(keys %$came_from);
            my $length2 = scalar(keys %$new_graph) - $length1;
            return $length1 * $length2;
        }
    }
}

open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my @input = <$fh>;
chomp @input;
close $fh;

print solve(\@input), "\n";
