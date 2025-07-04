
function main()
    fid = fopen('input.txt', 'r');
    changes = textscan(fid, '%d');
    fclose(fid);
    
    frequency = sum(cell2mat(changes));
    
    fprintf('%d\n', frequency);
end

main();
