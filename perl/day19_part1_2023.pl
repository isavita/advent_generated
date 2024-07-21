
use strict;
use warnings;

my %workflows;
my @parts;

sub parse_input {
    open my $fh, '<', 'input.txt' or die $!;
    while (<$fh>) {
        chomp;
        last if $_ eq '';
        my ($workflow_name, @rules) = parse_workflow($_);
        $workflows{$workflow_name} = \@rules;
    }
    while (<$fh>) {
        chomp;
        push @parts, parse_part($_);
    }
    close $fh;
}

sub parse_workflow {
    my ($line) = @_;
    my ($workflow_name, $rules_str) = $line =~ /^(.*?){(.*)}$/;
    my @rules = map { parse_rule($_) } split /,/, $rules_str;
    return ($workflow_name, @rules);
}

sub parse_rule {
    my ($rule_str) = @_;
    my %rule;
    if ($rule_str =~ /^(.)?(.)?(\d*):(.*)$/) {
        $rule{Category} = $1 // '';
        $rule{Operator} = $2 // '';
        $rule{Num} = $3;
        $rule{WorkflowName} = $4;
    } else {
        $rule{WorkflowName} = $rule_str;
    }
    return \%rule;
}

sub parse_part {
    my ($line) = @_;
    my %part;
    $line =~ /{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}/;
    @part{qw/x m a s/} = ($1, $2, $3, $4);
    return \%part;
}

sub apply_workflow {
    my ($part, $workflow_name) = @_;
    return 1 if $workflow_name eq 'A';
    return 0 if $workflow_name eq 'R';

    for my $rule (@{$workflows{$workflow_name}}) {
        my $rating = $part->{$rule->{Category}};
        my $is_valid = ($rule->{Operator} eq '>' ? $rating > $rule->{Num} :
                        $rule->{Operator} eq '<' ? $rating < $rule->{Num} : 1);
        return apply_workflow($part, $rule->{WorkflowName}) if $is_valid;
    }
    return 0;
}

sub solve {
    my $start_workflow = 'in';
    my $res = 0;

    for my $part (@parts) {
        if (apply_workflow($part, $start_workflow)) {
            $res += $_ for values %$part;
        }
    }
    return $res;
}

parse_input();
print solve();
