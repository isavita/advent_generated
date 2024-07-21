
use strict;
use warnings;
use List::Util qw(max);

# Read the input from the file
open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my @lines = <$fh>;
close $fh;

# Sort the log entries by timestamp
@lines = sort @lines;

my %sleep_data;
my $current_guard;
my $fall_asleep_time;

foreach my $line (@lines) {
    if ($line =~ /Guard #(\d+) begins shift/) {
        $current_guard = $1;
    } elsif ($line =~ /(\d{2}):(\d{2})] falls asleep/) {
        $fall_asleep_time = $2;
    } elsif ($line =~ /(\d{2}):(\d{2})] wakes up/) {
        my $wake_time = $2;
        for my $minute ($fall_asleep_time .. $wake_time - 1) {
            $sleep_data{$current_guard}{$minute}++;
        }
    }
}

# Find the guard with the most sleep
my ($most_asleep_guard, $max_sleep_time) = (undef, 0);
my %guard_sleep_totals;

foreach my $guard (keys %sleep_data) {
    my $total_sleep = 0;
    foreach my $minute (keys %{$sleep_data{$guard}}) {
        $total_sleep += $sleep_data{$guard}{$minute};
    }
    $guard_sleep_totals{$guard} = $total_sleep;
    if ($total_sleep > $max_sleep_time) {
        $max_sleep_time = $total_sleep;
        $most_asleep_guard = $guard;
    }
}

# Find the minute the most asleep guard is asleep the most
my $most_asleep_minute = (sort { $sleep_data{$most_asleep_guard}{$b} <=> $sleep_data{$most_asleep_guard}{$a} } keys %{$sleep_data{$most_asleep_guard}})[0];

# Calculate the result
my $result = $most_asleep_guard * $most_asleep_minute;

# Print the result
print "The result is: $result\n";
