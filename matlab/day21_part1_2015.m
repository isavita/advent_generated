
function main()
    % Item stats: [cost, damage, armor]
    weapons_data = [
        8, 4, 0;    % Dagger
        10, 5, 0;   % Shortsword
        25, 6, 0;   % Warhammer
        40, 7, 0;   % Longsword
        74, 8, 0    % Greataxe
    ];

    armor_data = [
        0, 0, 0;    % None
        13, 0, 1;   % Leather
        31, 0, 2;   % Chainmail
        53, 0, 3;   % Splintmail
        75, 0, 4;   % Bandedmail
        102, 0, 5   % Platemail
    ];

    rings_data = [
        0, 0, 0;    % None
        25, 1, 0;   % Damage +1
        50, 2, 0;   % Damage +2
        100, 3, 0;  % Damage +3
        20, 0, 1;   % Defense +1
        40, 0, 2;   % Defense +2
        80, 0, 3    % Defense +3
    ];

    % Boss stats: [Hit points, damage, armor]
    boss_hp = 103;
    boss_damage = 9;
    boss_armor = 2;

    % Player stats: Hit points (damage and armor are from items)
    player_hp = 100;

    min_gold = inf;

    num_weapons = size(weapons_data, 1);
    num_armor = size(armor_data, 1);
    num_rings = size(rings_data, 1);

    for i_w = 1:num_weapons
        weapon_cost = weapons_data(i_w, 1);
        weapon_damage = weapons_data(i_w, 2);
        weapon_armor = weapons_data(i_w, 3);

        for i_a = 1:num_armor
            armor_cost = armor_data(i_a, 1);
            armor_damage = armor_data(i_a, 2);
            armor_armor = armor_data(i_a, 3);

            % Iterate through unique pairs of rings (combinations)
            for i_r1 = 1:num_rings
                for i_r2 = i_r1 + 1:num_rings % Ensures distinct rings and avoids duplicates (A,B) vs (B,A)
                    ring1_cost = rings_data(i_r1, 1);
                    ring1_damage = rings_data(i_r1, 2);
                    ring1_armor = rings_data(i_r1, 3);

                    ring2_cost = rings_data(i_r2, 1);
                    ring2_damage = rings_data(i_r2, 2);
                    ring2_armor = rings_data(i_r2, 3);

                    current_cost = weapon_cost + armor_cost + ring1_cost + ring2_cost;
                    current_player_damage = weapon_damage + armor_damage + ring1_damage + ring2_damage;
                    current_player_armor = weapon_armor + armor_armor + ring1_armor + ring2_armor;

                    % Calculate turns to win/lose
                    player_effective_damage = max(1, current_player_damage - boss_armor);
                    boss_effective_damage = max(1, boss_damage - current_player_armor);

                    player_turns = ceil(boss_hp / player_effective_damage);
                    boss_turns = ceil(player_hp / boss_effective_damage);

                    % Check win condition
                    if player_turns <= boss_turns
                        min_gold = min(min_gold, current_cost);
                    end
                end
            end
        end
    end

    fprintf('%.0f\n', min_gold);
end
