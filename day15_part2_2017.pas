program Solution;

var
    input : text;
    genAStart, genBStart, genAFactor, genBFactor, modulus, genA, genB, matches, i : int64;

begin
    assign(input, 'input.txt');
    reset(input);

    readln(input, genAStart);
    readln(input, genBStart);

    genAFactor := 16807;
    genBFactor := 48271;
    modulus := 2147483647;

    genA := genAStart;
    genB := genBStart;
    matches := 0;

    for i := 0 to 4999999 do
    begin
        repeat
            genA := (genA * genAFactor) mod modulus;
        until genA mod 4 = 0;

        repeat
            genB := (genB * genBFactor) mod modulus;
        until genB mod 8 = 0;

        if (genA and $FFFF = genB and $FFFF) then
            matches := matches + 1;
    end;

    writeln(matches);
end.