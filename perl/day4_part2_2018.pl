use strict;
use warnings;

my $inputFile = "input.txt";
open(my $fh, '<', $inputFile) or die "Could not open file '$inputFile' $!";

my @records;
my %guards;

while (my $line = <$fh>) {
    chomp $line;
    my $time = substr($line, 1, 16);
    my $event = substr($line, 18);
    push @records, { time => $time, event => $event };
}

@records = sort { $a->{time} cmp $b->{time} } @records;

my $currentGuard;
my $sleepStart;

foreach my $record (@records) {
    if ($record->{event} =~ /begins shift/) {
        my $id = (split(' ', $record->{event}))[1];
        $id = substr($id, 1);
        if (!exists $guards{$id}) {
            $guards{$id} = { id => $id, minutes => [(0) x 60], totalMin => 0 };
        }
        $currentGuard = $guards{$id};
    } elsif ($record->{event} =~ /falls asleep/) {
        $sleepStart = (split(':', $record->{time}))[1];
    } elsif ($record->{event} =~ /wakes up/) {
        my $wakeUpMin = (split(':', $record->{time}))[1];
        for my $i ($sleepStart..$wakeUpMin-1) {
            $currentGuard->{minutes}[$i]++;
            $currentGuard->{totalMin}++;
        }
    }
}

my $mostFreqGuard;
my $mostFreqMin;

foreach my $id (keys %guards) {
    foreach my $i (0..59) {
        my $m = $guards{$id}->{minutes}[$i];
        if (!$mostFreqGuard || $m > $mostFreqGuard->{minutes}[$mostFreqMin]) {
            $mostFreqGuard = $guards{$id};
            $mostFreqMin = $i;
        }
    }
}

print $mostFreqGuard->{id} * $mostFreqMin . "\n";