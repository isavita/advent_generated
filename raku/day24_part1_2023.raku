
my @pts = 'input.txt'.IO.lines.map: { .split(/','|'@'/).map(*.Num)[0,1,2,3,4,5] }

sub cross(@a,@b) { @a[0]*@b[1] - @a[1]*@b[0] }

my $cnt = 0;
for @pts.kv -> $i, @p1 {
    for @pts[^$i] -> @p2 {
        my $det = @p1[3]*@p2[4] - @p2[3]*@p1[4];
        next if $det == 0;
        my $t1 = (@p2[4]*(@p2[0]-@p1[0]) - @p2[3]*(@p2[1]-@p1[1])) / $det;
        my $t2 = (@p1[4]*(@p2[0]-@p1[0]) - @p1[3]*(@p2[1]-@p1[1])) / $det;
        if $t1 >= 0 && $t2 >= 0 {
            my $x = @p1[0] + @p1[3]*$t1;
            my $y = @p1[1] + @p1[4]*$t1;
            if 2e14 <= $x <= 4e14 && 2e14 <= $y <= 4e14 {
                $cnt++
            }
        }
    }
}
put $cnt;
