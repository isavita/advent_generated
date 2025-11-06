
my %caves;

for 'input.txt'.IO.lines -> $line {
    my ($from, $to) = $line.split('-');
    %caves{$from}{$to} = True;
    %caves{$to}{$from} = True;
}

my Int $count = 0;
my &dfs;
&dfs = sub ($current, %visited) {
    if $current eq 'end' {
        $count++;
        return;
    }
    for %caves{$current}.keys -> $next {
        next if %visited{$next} && $next eq $next.lc;
        my %v = %visited;
        %v{$next} = True;
        &dfs($next, %v);
    }
}

dfs('start', %(start => True));
put $count;
