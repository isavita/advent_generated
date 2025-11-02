
#!/usr/bin/env raku
use v6;

sub MAIN {
    my $filename = "input.txt";
    my $fh = open $filename, :r or die "Could not open $filename: $!";
    my @events;
    
    for $fh.lines -> $line {
        chomp $line;
        my $pos = index($line, ']');
        next if $pos < 0;
        
        my $timestamp-str = $line.substr(0, $pos+1);
        my $event-str = $line.substr($pos+1).trim;
        
        @events.push({ :time($timestamp-str), :event($event-str) });
    }
    $fh.close;
    
    @events = @events.sort: { $^a<time> cmp $^b<time> };
    
    my %guard-stats;
    my $current-guard = 0;
    my $sleep-start = -1;
    
    for @events -> $event {
        my $action = $event<event>;
        if $action ~~ / 'Guard #' (\d+) ' begins shift'/ {
            $current-guard = $0.Int;
            $sleep-start = -1;
        }
        elsif $action ~~ / 'falls asleep'/ {
            my $time-str = $event<time>;
            my $minute = $time-str.comb(/\d+/).tail.Int;
            $sleep-start = $minute;
        }
        elsif $action ~~ / 'wakes up'/ {
            if $sleep-start != -1 {
                my $time-str = $event<time>;
                my $minute = $time-str.comb(/\d+/).tail.Int;
                
                unless %guard-stats{$current-guard} {
                    %guard-stats{$current-guard} = {
                        total-sleep => 0,
                        minutes => Array[Int].new(0 xx 60)
                    };
                }
                my $guard-hash = %guard-stats{$current-guard};
                for $sleep-start .. ($minute-1) -> $m {
                    $guard-hash<minutes>[$m]++;
                    $guard-hash<total-sleep>++;
                }
                $sleep-start = -1;
            }
        }
    }
    
    my $max-sleep = -1;
    my $best-guard = 0;
    for %guard-stats.keys -> $guard-id {
        my $stats = %guard-stats{$guard-id};
        if $stats<total-sleep> > $max-sleep {
            $max-sleep = $stats<total-sleep>;
            $best-guard = $guard-id;
        }
    }
    
    my $best-minute = 0;
    if $best-guard != 0 {
        my $max-minute-count = 0;
        for 0..59 -> $m {
            my $count = %guard-stats{$best-guard}<minutes>[$m];
            if $count > $max-minute-count {
                $max-minute-count = $count;
                $best-minute = $m;
            }
        }
        say $best-guard * $best-minute;
    }
    else {
        say 0;
    }
}
