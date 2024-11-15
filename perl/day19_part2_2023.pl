
#!/usr/bin/perl
use strict;
use warnings;
use feature 'say';

my %workflows;
my @parts;
my $min_rating = 1;
my $max_rating = 4000;

sub parse_workflow {
    my ($line) = @_;
    my ($name, $rules_str) = $line =~ /^(\w+)\{(.+)\}$/;
    my @rules_raw = split(',', $rules_str);
    
    my @rules;
    for my $rule_str (@rules_raw) {
        my $rule = {};
        if ($rule_str =~ /^(\w)([<>])(\d+):(\w+)$/) {
            $rule = {
                category => $1,
                operator => $2,
                num => $3,
                workflow_name => $4
            };
        } else {
            $rule = {workflow_name => $rule_str};
        }
        push @rules, $rule;
    }
    
    $workflows{$name} = \@rules;
}

sub parse_part {
    my ($line) = @_;
    my %part;
    ($part{x}, $part{m}, $part{a}, $part{s}) = $line =~ /x=(\d+),m=(\d+),a=(\d+),s=(\d+)/;
    return \%part;
}

sub apply_workflow_interval {
    my ($part_interval, $workflow_name) = @_;
    
    return 0 if $workflow_name eq 'R';
    
    if ($workflow_name eq 'A') {
        my $res = 1;
        $res *= $_->{end} - $_->{start} + 1 for values %$part_interval;
        return $res;
    }
    
    my $res = 0;
    for my $rule (@{$workflows{$workflow_name}}) {
        my $rating_interval = $part_interval->{$rule->{category}};
        my ($valid_interval, $invalid_interval);
        
        if ($rule->{operator} && $rule->{operator} eq '>') {
            $invalid_interval = {
                start => $rating_interval->{start},
                end => $rule->{num}
            };
            $valid_interval = {
                start => $rule->{num} + 1,
                end => $rating_interval->{end}
            };
        } elsif ($rule->{operator} && $rule->{operator} eq '<') {
            $valid_interval = {
                start => $rating_interval->{start},
                end => $rule->{num} - 1
            };
            $invalid_interval = {
                start => $rule->{num},
                end => $rating_interval->{end}
            };
        } else {
            $valid_interval = $rating_interval;
        }
        
        my %new_part = map { $_ => $part_interval->{$_} } keys %$part_interval;
        $new_part{$rule->{category}} = $valid_interval if $rule->{operator};
        
        $res += apply_workflow_interval(\%new_part, $rule->{workflow_name});
        
        $part_interval->{$rule->{category}} = $invalid_interval if $invalid_interval;
    }
    
    return $res;
}

# Read input
open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my @input = <$fh>;
chomp @input;
close $fh;

# Parse input
my $parsing_workflows = 1;
for my $line (@input) {
    last if $line eq '';
    parse_workflow($line);
}

# Prepare initial interval
my %part_interval = map { 
    $_ => {start => $min_rating, end => $max_rating} 
} qw(x m a s);

# Solve
my $result = apply_workflow_interval(\%part_interval, 'in');
say $result;
