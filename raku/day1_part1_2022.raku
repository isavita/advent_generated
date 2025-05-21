
sub MAIN {
    say slurp('input.txt').trim.split("\n\n").map({ .words.map(*.Int).sum }).max;
}
