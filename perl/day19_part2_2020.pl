
#!/usr/bin/perl
use strict;
use warnings;

sub solve {
    my ($input) = @_;
    my ($graph, $messages) = parse_input($input);

    fill_in_graph($graph, 42);
    fill_in_graph($graph, 31);

    my $part42 = join('|', @{$graph->{42}->{resolved}});
    my $part31 = join('|', @{$graph->{31}->{resolved}});

    my $rule8_string = "($part42)+";

    my $match_rule_zero = 0;
    for my $m (@$messages) {
        for my $i (1..10) {
            my $pattern = qr/^$rule8_string($part42){$i}($part31){$i}$/;
            if ($m =~ $pattern) {
                $match_rule_zero++;
                last;
            }
        }
    }
    return $match_rule_zero;
}

sub fill_in_graph {
    my ($graph, $entry) = @_;
    return @{$graph->{$entry}->{resolved}} if @{$graph->{$entry}->{resolved}};

    for my $option (@{$graph->{$entry}->{options}}) {
        my @resolved = ("");
        for my $entry_point (@$option) {
            my @nested_resolve_vals = fill_in_graph($graph, $entry_point);
            my @new_resolved;
            for my $next_piece (@nested_resolve_vals) {
                for my $prev (@resolved) {
                    push @new_resolved, $prev . $next_piece;
                }
            }
            @resolved = @new_resolved;
        }
        push @{$graph->{$entry}->{resolved}}, @resolved;
    }
    return @{$graph->{$entry}->{resolved}};
}

sub parse_input {
    my ($input) = @_;
    my ($rules_str, $messages_str) = split /\n\n/, $input;

    my %rules;
    for my $r (split /\n/, $rules_str) {
        if ($r =~ /([a-z])/) {
            my ($num, $char) = ($r =~ /^(\d+): "(\w)"/);
            $rules{$num} = { resolved => [$char] };
        } else {
            my ($key, $rule_nums) = ($r =~ /^(\d+): (.*)/);
            my @options;
            for my $rule_num_set (split / \| /, $rule_nums) {
                push @options, [split / /, $rule_num_set];
            }
            $rules{$key} = { options => \@options, resolved => [] };
        }
    }
    my @messages = split /\n/, $messages_str;
    return \%rules, \@messages;
}

my $file_content;
{
    open my $fh, "<", "input.txt" or die "Could not open file: $!";
    local $/ = undef;
    $file_content = <$fh>;
    close $fh;
}
$file_content =~ s/^\s+|\s+$//g;
print solve($file_content), "\n";
