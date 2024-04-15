#!/usr/bin/perl

use strict;
use warnings;

sub item_priority {
    my $item = shift;
    return ord($item) - (($item =~ /[a-z]/) ? ord('a') - 1 : ord('A') - 27);
}

open my $file, '<', 'input.txt' or die "Error opening file: $!";

my $sum = 0;
my @group_items;
my $group_line_counter = 0;

while (my $line = <$file>) {
    chomp $line;
    my %items_map;
    $items_map{$_}++ for split //, $line;
    $group_items[$group_line_counter] = \%items_map;
    $group_line_counter++;

    if ($group_line_counter == 3) {
        my %common_items;
        for my $item (keys %{$group_items[0]}) {
            if ($group_items[1]{$item} && $group_items[2]{$item}) {
                $common_items{$item}++;
            }
        }
        for my $item (keys %common_items) {
            $sum += item_priority($item);
            last; # Since we need only one common item per group
        }
        $group_line_counter = 0;
        @group_items = ();
    }
}

close $file;

print "$sum\n";