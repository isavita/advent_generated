
function main()
    fid = fopen('input.txt', 'r');
    modules = fscanf(fid, '%d');
    fclose(fid);

    totalFuel = sum(arrayfun(@calculateFuel, modules));
    disp(totalFuel);
end

function fuel = calculateFuel(mass)
    fuel = floor(mass / 3) - 2;
    if fuel <= 0
        fuel = 0;
    else
        fuel = fuel + calculateFuel(fuel);
    end
end

main();
