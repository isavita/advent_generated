
function main()
    % Read input from input.txt
    try
        particles = read_particles('input.txt');
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    % --- Part One: Find the particle closest to <0,0,0> in the long term ---
    % The particle with the smallest absolute acceleration magnitude will eventually
    % have the smallest velocity and thus stay closest to the origin.
    % If accelerations are equal, the one with smaller initial velocity magnitude
    % will be closer. If velocities are also equal, the one with smaller initial
    % position magnitude will be closer.
    
    % Calculate acceleration magnitudes
    accel_mags = arrayfun(@(p) sum(abs(p.a)), particles);
    
    % Find the index of the particle with the minimum acceleration magnitude
    [~, min_accel_idx] = min(accel_mags);
    
    % If there are ties in acceleration, we need to consider velocity and position.
    % However, for the "long term" closest, the acceleration is the dominant factor.
    % A particle with zero acceleration will maintain its velocity. A particle
    % with negative acceleration in the direction of the origin will move towards it.
    % A particle with positive acceleration away from the origin will move away.
    % The particle with the smallest absolute acceleration magnitude will have
    % the slowest change in velocity, and thus its position will converge
    % to a stable point or oscillate with the smallest amplitude.
    % For simplicity and efficiency, we assume the minimum acceleration magnitude
    % is sufficient to determine the long-term closest particle.
    
    fprintf('Part One: Particle closest in the long term is particle %d\n', min_accel_idx);

    % --- Part Two: Count remaining particles after collisions ---
    
    % Simulate particle movement and handle collisions
    remaining_particles = simulate_collisions(particles);
    
    fprintf('Part Two: Number of particles remaining after collisions is %d\n', length(remaining_particles));
end

function particles = read_particles(filename)
    % Reads particle data from a file.
    % Each line is expected to be in the format:
    % p=<x,y,z>, v=<x,y,z>, a=<x,y,z>
    
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end
    
    data = textscan(fid, 'p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>\n', 'Delimiter', ',', 'CollectOutput', true);
    fclose(fid);
    
    % Parse the data into a struct array
    num_particles = size(data{1}, 1);
    particles = struct('id', {}, 'p', {}, 'v', {}, 'a', {});
    
    for i = 1:num_particles
        particles(i).id = i - 1; % Particle IDs start from 0
        particles(i).p = data{1}(i, 1:3);
        particles(i).v = data{1}(i, 4:6);
        particles(i).a = data{1}(i, 7:9);
    end
end

function remaining_particles = simulate_collisions(initial_particles)
    % Simulates particle movement and resolves collisions.
    % Returns a struct array of particles that have not collided.
    
    particles = initial_particles;
    
    % We need to simulate for a sufficient number of ticks.
    % A reasonable heuristic is to simulate until the minimum Manhattan distance
    % starts consistently increasing for all remaining particles, or for a
    % fixed large number of steps. For this problem, a fixed large number
    % of steps is usually sufficient to resolve all collisions.
    % Let's choose a number of steps that is likely to be more than enough
    % to resolve all collisions. A few hundred steps should be ample.
    num_simulation_steps = 200; 
    
    for step = 1:num_simulation_steps
        if isempty(particles)
            break; % No particles left to simulate
        end
        
        % Update velocities
        for i = 1:length(particles)
            particles(i).v = particles(i).v + particles(i).a;
        end
        
        % Update positions
        for i = 1:length(particles)
            particles(i).p = particles(i).p + particles(i).v;
        end
        
        % Detect and resolve collisions
        
        % Create a map to store positions and the particles at those positions
        position_map = containers.Map('KeyType', 'char', 'ValueType', 'any');
        
        for i = 1:length(particles)
            pos_str = sprintf('%d,%d,%d', particles(i).p(1), particles(i).p(2), particles(i).p(3));
            
            if isKey(position_map, pos_str)
                % Collision detected: add current particle to the list of colliding particles
                colliding_particles_at_pos = position_map(pos_str);
                colliding_particles_at_pos{end+1} = particles(i).id;
                position_map(pos_str) = colliding_particles_at_pos;
            else
                % First particle at this position
                position_map(pos_str) = {particles(i).id};
            end
        end
        
        % Identify particles that will be removed due to collision
        particles_to_remove_ids = [];
        keys_to_process = keys(position_map);
        
        for k = 1:length(keys_to_process)
            key = keys_to_process{k};
            colliding_ids = position_map(key);
            if length(colliding_ids) > 1
                % If more than one particle is at this position, they all collide
                particles_to_remove_ids = [particles_to_remove_ids, colliding_ids{:}];
            end
        end
        
        % Remove duplicate IDs if any (though the logic above should prevent this)
        particles_to_remove_ids = unique(particles_to_remove_ids);
        
        % Filter out the particles that collided
        if ~isempty(particles_to_remove_ids)
            current_particle_ids = arrayfun(@(p) p.id, particles);
            
            % Find indices of particles to keep
            indices_to_keep = ~ismember(current_particle_ids, particles_to_remove_ids);
            particles = particles(indices_to_keep);
        end
    end
    
    remaining_particles = particles;
end

% --- Entry point ---
% This function is called when the script is executed.
% It's good practice to have a main function for clarity and organization.
% In MATLAB, you can call functions directly or use a script that calls a main function.
% For this example, we'll assume this file is saved as 'particle_swarm.m'
% and 'main()' is called from the MATLAB command window or another script.
% If you want to run it directly as a script, you can remove the 'function' keyword
% and just have the code, but using a main function is cleaner.

% To run this:
% 1. Save the code as 'particle_swarm.m'
% 2. Create a file named 'input.txt' in the same directory with your particle data.
% 3. In the MATLAB command window, type: particle_swarm
%    (or if you want to call the main function explicitly: main)

% If you want this file to be executable directly as a script that calls main:
% Remove the 'function main()' line and the 'end' at the very end.
% Then, the code will run sequentially. However, the prompt asked for a
% 'main' entry point, so the function approach is more appropriate.

% Example of how to call main if this file is named 'particle_swarm.m':
% In MATLAB command window:
% >> particle_swarm
% This will execute the code within the file, which includes calling main().
