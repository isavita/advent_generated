
function solve()
    % Read input from "input.txt"
    fid = fopen('input.txt', 'r');
    boss_hp = sscanf(fgetl(fid), 'Hit Points: %d');
    boss_damage = sscanf(fgetl(fid), 'Damage: %d');
    boss_armor = sscanf(fgetl(fid), 'Armor: %d');
    fclose(fid);

    % Item data: [cost, damage, armor]
    % Weapons provide damage
    weapons = [
        8, 4, 0;
        10, 5, 0;
        25, 6, 0;
        40, 7, 0;
        74, 8, 0
    ];

    % Armors provide armor
    armors = [
        0, 0, 0;   % No armor
        13, 0, 1;
        31, 0, 2;
        53, 0, 3;
        75, 0, 4;
        102, 0, 5
    ];

    % Rings provide damage or armor
    rings_data = [
        0, 0, 0;   % Empty ring (for combinations where one or both ring slots are empty)
        25, 1, 0;  % Damage +1
        50, 2, 0;  % Damage +2
        100, 3, 0; % Damage +3
        20, 0, 1;  % Armor +1
        40, 0, 2;  % Armor +2
        80, 0, 3   % Armor +3
    ];

    player_base_hp = 100;
    max_cost = 0;

    num_weapons = size(weapons, 1);
    num_armors = size(armors, 1);
    num_rings = size(rings_data, 1);

    % Iterate through all combinations of equipment
    for i_w = 1:num_weapons
        w_cost = weapons(i_w, 1);
        w_damage = weapons(i_w, 2);

        for i_a = 1:num_armors
            a_cost = armors(i_a, 1);
            a_armor = armors(i_a, 3);

            for i_r1 = 1:num_rings
                r1_cost = rings_data(i_r1, 1);
                r1_damage = rings_data(i_r1, 2);
                r1_armor = rings_data(i_r1, 3);

                for i_r2 = (i_r1 + 1):num_rings % Ensure distinct rings (i_r2 > i_r1)
                    r2_cost = rings_data(i_r2, 1);
                    r2_damage = rings_data(i_r2, 2);
                    r2_armor = rings_data(i_r2, 3);

                    % Calculate current player stats and total cost
                    current_player_damage = w_damage + r1_damage + r2_damage;
                    current_player_armor = a_armor + r1_armor + r2_armor;
                    current_cost = w_cost + a_cost + r1_cost + r2_cost;

                    % Check if player loses and if this cost is the maximum so far
                    if ~player_wins(player_base_hp, current_player_damage, current_player_armor, ...
                                    boss_hp, boss_damage, boss_armor)
                        if current_cost > max_cost
                            max_cost = current_cost;
                        end
                    end
                end
            end
        end
    end

    % Print the result
    fprintf('%d\n', max_cost);
end

% Helper function to determine if the player wins the combat
function result = player_wins(player_hp, player_damage, player_armor, boss_hp, boss_damage, boss_armor)
    % Calculate effective damage for both player and boss
    player_effective_damage = max(1, player_damage - boss_armor);
    boss_effective_damage = max(1, boss_damage - player_armor);

    % Calculate turns needed to defeat opponent
    player_turns = ceil(boss_hp / player_effective_damage);
    boss_turns = ceil(player_hp / boss_effective_damage);

    % Player wins if they defeat the boss in fewer or equal turns
    result = player_turns <= boss_turns;
end
