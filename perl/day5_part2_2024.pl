
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Could not open file: $!";

my %rules;
my @updates;

# Parse input
while (<$fh>) {
    chomp;
    if ($_ =~ /(\d+)\|(\d+)/) {
        $rules{$1}{$2} = 1;
    } elsif ($_ =~ /\d/) {
        push @updates, [split ',', $_];
    }
}

close $fh;

sub is_ordered {
    my ($update, $rules) = @_;
    my %update_map = map { $_ => 1 } @$update;
    for my $i (0 .. $#$update) {
        for my $j ($i + 1 .. $#$update) {
            if (exists $rules->{$update->[$j]} && exists $rules->{$update->[$j]}->{$update->[$i]}
               && exists $update_map{$update->[$j]} && exists $update_map{$update->[$i]}) {
                return 0;
            }
        }
    }
    return 1;
}

sub topological_sort {
    my ($update, $rules) = @_;
    my @sorted;
    my %in_degree;
    my %update_map = map { $_ => 1 } @$update;

    # Calculate in-degrees
    foreach my $page (@$update) {
        $in_degree{$page} = 0;
    }
    foreach my $page (@$update) {
      foreach my $next (@$update){
        if (exists $rules->{$page} && exists $rules->{$page}->{$next}
          && exists $update_map{$page} && exists $update_map{$next}) {
          $in_degree{$next}++;
        }
      }
    }

    # Find nodes with in-degree 0
    my @queue = grep { $in_degree{$_} == 0 } @$update;

    while (@queue) {
        my $node = shift @queue;
        push @sorted, $node;

        foreach my $neighbor (@$update) {
            if (exists $rules->{$node} && exists $rules->{$node}->{$neighbor}
                && exists $update_map{$node} && exists $update_map{$neighbor}) {
                $in_degree{$neighbor}--;
                if ($in_degree{$neighbor} == 0) {
                    push @queue, $neighbor;
                }
            }
        }
    }

    return @sorted;
}

my $sum_ordered = 0;
my $sum_unordered = 0;
my @unordered_updates;

foreach my $update (@updates) {
    if (is_ordered($update, \%rules)) {
        $sum_ordered += $update->[int(@$update / 2)];
    } else {
        push @unordered_updates, $update;
    }
}

foreach my $update (@unordered_updates) {
  my @sorted = topological_sort($update, \%rules);
  $sum_unordered += $sorted[int(@sorted / 2)];
}

print "Part 1: $sum_ordered\n";
print "Part 2: $sum_unordered\n";
