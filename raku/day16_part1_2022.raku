
class Valve { has Str $.id; has Int $.flow; has Int %.tunnels }

sub MAIN {
    my %valves = 'input.txt'.IO.lines.map(-> \l {
        my ($id,$flow) = l.match(/Valve\s+(\w+).*?(\d+)/).list».Str;
        my Valve $v .= new(:$id, :flow($flow.Int));
        $v.tunnels{$_} = 1 for l.split(/lead.*?valve.?s?/).tail.comb(/\w+/);
        $id => $v
    });

    # Floyd-Warshall
    for %valves.keys -> \k {
        for %valves.keys -> \i {
            for %valves.keys -> \j {
                my \dik = %valves{i}.tunnels{k} // +Inf;
                my \dkj = %valves{k}.tunnels{j} // +Inf;
                my \dij = %valves{i}.tunnels{j} // +Inf;
                %valves{i}.tunnels{j} = (dij, dik+dkj).min if dik < +Inf && dkj < +Inf;
            }
        }
    }

    my \open = %valves.values.grep(*.flow > 0)».id.Set;

    sub max-pressure(Str \curr, Int \minute, Int \pressure, \open) {
        my Int $max = pressure;
        for open.keys -> \next-valve {
            my Int \time-left = minute - %valves{curr}.tunnels{next-valve} - 1;
            if time-left > 0 {
                $max = ($max, max-pressure(next-valve, time-left, pressure + time-left * %valves{next-valve}.flow, open ∖ next-valve)).max;
            }
        }
        $max
    }

    say max-pressure('AA', 30, 0, open)
}
