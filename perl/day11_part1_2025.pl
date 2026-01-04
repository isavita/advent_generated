
use strict;
use warnings;
use feature 'say';

sub count_paths {
    my ($cur,$target,$adj,$memo)=@_;
    return 1 if $cur eq $target;
    return $memo->{$cur} if exists $memo->{$cur};
    my $total=0;
    if (my $neighbors=$adj->{$cur}) {
        $total += count_paths($_,$target,$adj,$memo) for @$neighbors;
    }
    $memo->{$cur}=$total;
    $total
}

sub main {
    open my $fh,'<','input.txt' or die $!;
    my %adj;
    while (my $line=<$fh>) {
        chomp $line;
        $line=~s/^\s+|\s+$//g;
        next unless $line;
        my ($src,$dsts)=split /:/, $line, 2;
        next unless defined $dsts;
        $src=~s/^\s+|\s+$//g;
        $dsts=~s/^\s+|\s+$//g;
        my @dest=split /\s+/, $dsts;
        $adj{$src}=\@dest;
    }
    close $fh;
    my %memo;
    my $count=count_paths('you','out',\%adj,\%memo);
    say $count;
}
main();
