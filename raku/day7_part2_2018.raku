
sub MAIN() {
    my %tasks;
    my %indegree;
    for 'input.txt'.IO.lines {
        if /Step\s+(\w)\s+must\s+be\s+finished\s+before\s+step\s+(\w)\s+can\s+begin\./ {
            my $prereq = $0;
            my $step = $1;
            %tasks{$prereq}.push($step);
            %indegree{$step}++;
            %indegree{$prereq} //= 0;
        }
    }

    my $base-time = 60;
    my $worker-count = 5;
    my @available = %indegree.keys.grep({ %indegree{$_} == 0 }).sort.reverse;
    my %in-progress;
    my $time-elapsed = 0;

    while @available or %in-progress {
        while @available and %in-progress.elems < $worker-count {
            my $task = @available.pop;
            %in-progress{$task} = $base-time + $task.ord - 'A'.ord + 1;
        }

        my $min-time-left = %in-progress.values.min;
        $time-elapsed += $min-time-left;

        my @completed;
        for %in-progress.kv -> $task, $time {
            my $new-time = $time - $min-time-left;
            %in-progress{$task} = $new-time;
            if $new-time <= 0 {
                @completed.push($task);
                %in-progress{$task}:delete;
            }
        }

        for @completed -> $task {
            for %tasks{$task}.List -> $next {
                %indegree{$next}--;
                if %indegree{$next} == 0 {
                    @available.push($next);
                }
            }
            @available = @available.sort.reverse;
        }
    }

    say $time-elapsed;
}
