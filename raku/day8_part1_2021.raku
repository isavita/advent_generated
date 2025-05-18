
sub MAIN() {
    say 'input.txt'.IO.slurp.lines.map(*.split(" | ")[1].split(" ")).flat.grep({ .chars eq any <2 3 4 7> }).elems;
}
