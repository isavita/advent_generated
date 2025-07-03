
function main()
    fileId = fopen('input.txt', 'r');
    num_elves = str2double(fgetl(fileId));
    fclose(fileId);

    result = josephus(num_elves);
    fprintf('%d\n', result);
end

function result = josephus(n)
    binary_n = dec2bin(n);
    shifted_binary = [binary_n(3:end), '1'];
    result = bin2dec(shifted_binary);
end

main();
