
function main()
    fid = fopen('input.txt', 'r');
    masses = fscanf(fid, '%d');
    fclose(fid);
    total_fuel = sum(floor(masses / 3) - 2);
    disp(total_fuel);
end

main();
