
using Base: lcm

const BROADCASTER = 0
const FLIP_FLOP = 1
const CONJUNCTION = 2

abstract type AbstractModule end

mutable struct FlipFlop <: AbstractModule
    name::String
    module_type::Int
    connects_to::Vector{String}
    state::Bool
end

mutable struct Conjunction <: AbstractModule
    name::String
    module_type::Int
    connects_to::Vector{String}
    watches::Dict{String, Bool}
end

struct Broadcaster <: AbstractModule
    name::String
    module_type::Int
    connects_to::Vector{String}
end

function handle_line!(line::String, connections::Dict{String, AbstractModule})
    parts = split(line, " -> ")
    targets = split(parts[2], ", ")

    if parts[1] == "broadcaster"
        module_obj = Broadcaster(parts[1], BROADCASTER, targets)
        connections[module_obj.name] = module_obj
    elseif startswith(parts[1], '%')
        name = parts[1][2:end]
        module_obj = FlipFlop(name, FLIP_FLOP, targets, false)
        connections[name] = module_obj
    else # Must be Conjunction
        name = parts[1][2:end]
        module_obj = Conjunction(name, CONJUNCTION, targets, Dict{String, Bool}())
        connections[name] = module_obj
    end
end

function complete_watches!(connections::Dict{String, AbstractModule})
    conjunction_names = String[]
    inputs_map = Dict{String, Vector{String}}()

    for (name, module_obj) in connections
        if module_obj.module_type == CONJUNCTION
            push!(conjunction_names, name)
        end
        for target_name in module_obj.connects_to
             if !haskey(inputs_map, target_name)
                 inputs_map[target_name] = String[]
             end
             push!(inputs_map[target_name], name)
        end
    end

     for conj_name in conjunction_names
         if haskey(inputs_map, conj_name)
             conj_module = connections[conj_name]
             for input_name in inputs_map[conj_name]
                 conj_module.watches[input_name] = false
             end
         end
     end
end

struct State
    from::String
    name::String
    pulse::Bool # false = low, true = high
end

function simulate_press!(connections::Dict{String, AbstractModule}, loops::Dict{String, Int}, press_number::Int)
    queue = State[]
    push!(queue, State("button", "broadcaster", false))
    found_rx_low = false

    while !isempty(queue)
        curr_state = popfirst!(queue)

        if !haskey(connections, curr_state.name)
             if curr_state.name == "rx" && !curr_state.pulse
                 found_rx_low = true
             end
            continue
        end

        module_obj = connections[curr_state.name]
        pulse = curr_state.pulse

        if module_obj.module_type == BROADCASTER
            for name in module_obj.connects_to
                push!(queue, State(module_obj.name, name, pulse))
            end
        elseif module_obj.module_type == FLIP_FLOP
            if !pulse # Only trigger on low pulse
                module_obj.state = !module_obj.state
                for name in module_obj.connects_to
                    push!(queue, State(module_obj.name, name, module_obj.state))
                end
            end
        elseif module_obj.module_type == CONJUNCTION
            module_obj.watches[curr_state.from] = pulse
            all_high = all(values(module_obj.watches))
            output_pulse = !all_high

             # Check for loops needed for part 2
             if haskey(loops, curr_state.name) && output_pulse && loops[curr_state.name] == -1
                 loops[curr_state.name] = press_number
             end

            for name in module_obj.connects_to
                push!(queue, State(module_obj.name, name, output_pulse))
            end
        end
    end
    return found_rx_low
end

function find_feeder(target::String, connections::Dict{String, AbstractModule})
     for (name, mod) in connections
         if target in mod.connects_to
             return name
         end
     end
     error("No module found feeding into $target")
 end


function main()
    lines = readlines("input.txt")

    connections = Dict{String, AbstractModule}()
    for line in lines
        handle_line!(line, connections)
    end

    complete_watches!(connections)

    rx_feeder_name = ""
    try
        rx_feeder_name = find_feeder("rx", connections)
    catch e
        println("rx module or its feeder not found, might be running Part 1 logic or input differs.")
        # If rx or feeder isn't there, we can't run part 2 logic.
        # Fallback or specific error handling could go here if needed.
        # For this problem, it implies rx isn't a target or has no direct input.
        # We'll assume the structure needed for Part 2 exists based on the problem context.
         if !("rx" in keys(connections)) && rx_feeder_name == ""
             println("Assuming Part 1 is needed - but this code solves Part 2's structure.")
             # Calculate Part 1 if necessary (sum_history logic would go here)
             # The provided python code structure directly targets Part 2's LCM approach
             return # Exit if the expected structure for Part 2 isn't present
         end
    end


    if !(haskey(connections, rx_feeder_name) && connections[rx_feeder_name].module_type == CONJUNCTION)
         error("Error: Module feeding 'rx' ($rx_feeder_name) is not a conjunction or does not exist.")
    end

    final_conj = connections[rx_feeder_name]
    loop_lengths = Dict{String, Int}(name => -1 for name in keys(final_conj.watches))

    press_number = 0
    while true
        press_number += 1
        simulate_press!(connections, loop_lengths, press_number)

        if all(len != -1 for len in values(loop_lengths))
            break
        end
        # Add a safeguard against infinite loops if inputs don't cycle as expected
        if press_number > 1_000_000 # Arbitrary large number
            error("Stopped after too many iterations, cycles not found for all inputs.")
        end
    end

    result = 1
    if !isempty(loop_lengths)
        result = reduce(lcm, values(loop_lengths))
    end

    println(result)
end

main()
