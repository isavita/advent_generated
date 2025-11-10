
#!/usr/bin/env raku

use v6;

# Read and sort the input lines
my $fh = open "input.txt", :r or die "Unable to open input file: $!";
my @lines = $fh.lines.sort;
$fh.close;

# Guard data: each guard is a hash with a 60‑element sleep array and total minutes
my %guards;
my $current;    # ID of the guard currently on duty
my $asleep;     # minute when the current guard fell asleep

sub ensure-guard ($gid) {
    %guards{$gid} //= { sleep => [0 xx 60], total => 0 };
}

# Process each log entry
for @lines -> $line {
    # Extract the minute value (always present)
    my $minute = +$0 if $line ~~ / ':' (\d+) ']' /;

    # Guard begins shift
    if $line ~~ / '#' (\d+) / {
        $current = $0;
        ensure-guard($current);
    }
    # Guard falls asleep
    elsif $line ~~ / 'falls asleep' / {
        $asleep = $minute;
    }
    # Guard wakes up
    elsif $line ~~ / 'wakes up' / {
        next unless $current.defined && $asleep.defined;
        my $g = %guards{$current};
        for $asleep .. $minute-1 -> $m {
            $g<sleep>[$m]++;
            $g<total>++;
        }
        $asleep = Any;
    }
}

# Part 1 – guard with most total sleep
my $sleepiest-guard = 0;
my $most-total = -1;
for %guards.kv -> $gid, $g {
    if $g<total> > $most-total {
        $most-total = $g<total>;
        $sleepiest-guard = $gid;
    }
}
my $most-minute = 0;
my $most-count = 0;
if $sleepiest-guard {
    for 0..59 -> $m {
        my $cnt = %guards{$sleepiest-guard}<sleep>[$m];
        if $cnt > $most-count {
            $most-count = $cnt;
            $most-minute = $m;
        }
    }
    say $sleepiest-guard * $most-minute;
}
else { say 0 }

# Part 2 – minute with highest frequency across all guards
my $max-freq = 0;
my $max-freq-guard = 0;
my $max-freq-minute = 0;
for %guards.kv -> $gid, $g {
    for 0..59 -> $m {
        my $cnt = $g<sleep>[$m];
        if $cnt > $max-freq {
            $max-freq = $cnt;
            $max-freq-guard = $gid;
            $max-freq-minute = $m;
        }
    }
}
say $max-freq-guard * $max-freq-minute;
