
use strict;
use warnings;

my %caves;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";

while (my $line = <$fh>) {
    chomp $line;
    my ($from, $to) = split('-', $line);

    if (!exists $caves{$from}) {
        $caves{$from} = {};
    }

    if (!exists $caves{$to}) {
        $caves{$to} = {};
    }

    $caves{$from}{$to} = 1;
    $caves{$to}{$from} = 1;
}

my $count = 0;

sub dfs {
    my ($current, $visited) = @_;

    if ($current eq "end") {
        $count++;
        return;
    }

    foreach my $next (keys %{$caves{$current}}) {
        if ($visited->{$next} && lc($next) eq $next) {
            next;
        }

        my %visited_copy = %$visited;
        $visited_copy{$next} = 1;
        dfs($next, \%visited_copy);
    }
}

dfs("start", {"start" => 1});
print "$count\n";
