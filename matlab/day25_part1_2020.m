
function main()
    fid = fopen('input.txt', 'r');
    data = fscanf(fid, '%d');
    fclose(fid);

    cardPublicKey = data(1);
    doorPublicKey = data(2);

    cardLoopSize = findLoopSize(cardPublicKey);
    doorLoopSize = findLoopSize(doorPublicKey);

    encryptionKey1 = transformSubjectNumber(doorPublicKey, cardLoopSize);

    fprintf('%d\n', encryptionKey1);
end

function loopSize = findLoopSize(publicKey)
    value = 1;
    subjectNumber = 7;
    loopSize = 0;
    while value ~= publicKey
        loopSize = loopSize + 1;
        value = mod(value * subjectNumber, 20201227);
    end
end

function transformedValue = transformSubjectNumber(subjectNumber, loopSize)
    transformedValue = 1;
    for i = 1:loopSize
        transformedValue = mod(transformedValue * subjectNumber, 20201227);
    end
end

main();
