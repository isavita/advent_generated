
use strict;
use warnings;

my %graph;
my @best_clique;

open my $fh, '<', 'input.txt' or die "Error opening input file: $!";

my %nodes_set;
while (my $line = <$fh>) {
    chomp $line;
    my ($a, $b) = split /-/, $line;
    next unless defined $a && defined $b;
    $graph{$a}{$b} = 1;
    $graph{$b}{$a} = 1;
    $nodes_set{$a} = 1;
    $nodes_set{$b} = 1;
}
close $fh;

my @all_nodes = keys %nodes_set;

sub neighbors_of {
    my ($node) = @_;
    return $graph{$node} || {};
}

sub intersect {
    my ($a, $b) = @_;
    my @out;
    for my $x (@$a) {
        push @out, $x if $b->{$x};
    }
    return \@out;
}

sub union {
    my ($a, $x) = @_;
    return [@$a, $x];
}

sub remove {
    my ($slice, $s) = @_;
    return [grep { $_ ne $s } @$slice];
}

sub bron_kerbosch {
    my ($r, $p, $x) = @_;
    if (@$p == 0 && @$x == 0) {
        if (@$r > @best_clique) {
            @best_clique = @$r;
        }
        return;
    }

    my @temp_p = @$p;
    for my $v (@temp_p) {
        my $neighbors = neighbors_of($v);
        bron_kerbosch(
            union($r, $v),
            intersect($p, $neighbors),
            intersect($x, $neighbors),
        );
        $p = remove($p, $v);
        $x = union($x, $v);
    }
}

bron_kerbosch([], \@all_nodes, []);
@best_clique = sort @best_clique;
print join ',', @best_clique;
print "\n";
