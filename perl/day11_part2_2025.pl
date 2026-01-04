
use strict;
use warnings;
use autodie;

open my $fh, '<', 'input.txt';
my %adj;
while (my $line = <$fh>) {
    chomp $line;
    $line =~ s/^\s+|\s+$//g;
    next unless $line;
    my ($src, $dsts) = split /:/, $line, 2;
    next unless defined $dsts;
    $src =~ s/^\s+|\s+$//g;
    $dsts =~ s/^\s+|\s+$//g;
    my @dst = split /\s+/, $dsts;
    $adj{$src} = \@dst;
}
close $fh;

sub count_paths {
    my ($start, $end, $adj_ref) = @_;
    my %memo;
    my $dfs;
    $dfs = sub {
        my ($cur) = @_;
        return 1 if $cur eq $end;
        return $memo{$cur} if exists $memo{$cur};
        my $total = 0;
        if (my $neighbors = $adj_ref->{$cur}) {
            $total += $dfs->($_) for @$neighbors;
        }
        $memo{$cur} = $total;
        return $total;
    };
    return $dfs->($start);
}

my $s1 = count_paths('svr','dac',\%adj) *
         count_paths('dac','fft',\%adj) *
         count_paths('fft','out',\%adj);

my $s2 = count_paths('svr','fft',\%adj) *
         count_paths('fft','dac',\%adj) *
         count_paths('dac','out',\%adj);

print "Paths (svr->dac->fft->out): $s1\n";
print "Paths (svr->fft->dac->out): $s2\n";
print "Total paths visiting both: ", $s1 + $s2, "\n";
