
function main
    fid = fopen('input.txt','r');
    if fid<0, error('file'); end
    genAStart = fscanf(fid,'%d',1);
    genBStart = fscanf(fid,'%d',1);
    fclose(fid);

    genAFactor = 16807;
    genBFactor = 48271;
    modulus = 2147483647;

    genA = genAStart;
    genB = genBStart;
    matches = 0;

    for i = 1:5000000
        while true
            genA = mod(genA*genAFactor, modulus);
            if mod(genA,4)==0, break; end
        end
        while true
            genB = mod(genB*genBFactor, modulus);
            if mod(genB,8)==0, break; end
        end
        if bitand(genA,65535) == bitand(genB,65535)
            matches = matches + 1;
        end
    end

    disp(matches);
end
