
function solve()
    initial_state = strtrim(fileread('input.txt'));

    disk_length_1 = 272;
    disk_data_1 = generate_data(initial_state, disk_length_1);
    checksum_1 = calculate_checksum(disk_data_1);
    fprintf('%s\n', checksum_1);

    disk_length_2 = 35651584;
    disk_data_2 = generate_data(initial_state, disk_length_2);
    checksum_2 = calculate_checksum(disk_data_2);
    fprintf('%s\n', checksum_2);
end

function data = generate_data(initial_state, disk_length)
    data = initial_state;
    while length(data) < disk_length
        data = dragon_curve(data);
    end
    data = data(1:disk_length);
end

function b_flipped = flip_bits(a)
    b_flipped = char('1' - (a - '0'));
end

function result = dragon_curve(a)
    b = fliplr(a);
    b_flipped = flip_bits(b);
    result = [a '0' b_flipped];
end

function checksum = calculate_checksum(data)
    while mod(length(data), 2) == 0
        data_numeric = data - '0';
        data_reshaped = reshape(data_numeric, 2, []);
        checksum_numeric = (data_reshaped(1,:) == data_reshaped(2,:));
        data = char(checksum_numeric + '0');
    end
    checksum = data;
end
