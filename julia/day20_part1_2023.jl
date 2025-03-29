
using DataStructures: Queue

# --- Module Type Definitions ---

abstract type AbstractModule end

# Stores the state (on/off)
mutable struct FlipFlopModule <: AbstractModule
    name::String
    outputs::Vector{String}
    state::Bool # false = off, true = on
end

# Stores the memory of last pulse from each input
mutable struct ConjunctionModule <: AbstractModule
    name::String
    outputs::Vector{String}
    inputs::Dict{String,Bool} # input_name => last_pulse (false=low, true=high)
end

# Simple broadcaster, no state needed beyond definition
struct BroadcastModule <: AbstractModule
    name::String
    outputs::Vector{String}
end

# Represents modules that only receive (like 'output' or 'rx' if present)
# Or modules mentioned as outputs but not defined on the left-hand side.
struct UntypedModule <: AbstractModule
    name::String
    outputs::Vector{String} # Typically empty
end

# --- Parsing Logic ---

function parse_line(line::AbstractString)
    name_part, outputs_part = split(line, " -> ")
    outputs = strip.(split(outputs_part, ", "))

    if name_part == "broadcaster"
        return BroadcastModule(name_part, outputs)
    elseif startswith(name_part, '%')
        name = name_part[2:end]
        # Initial state is off (false)
        return FlipFlopModule(name, outputs, false)
    elseif startswith(name_part, '&')
        name = name_part[2:end]
        # Initial memory is empty, will be populated later
        return ConjunctionModule(name, outputs, Dict{String,Bool}())
    else
        # Should not happen based on problem description for sending modules,
        # but handle potential untyped definitions if necessary.
        # In this problem, only broadcaster lacks a prefix.
        # We'll treat any unexpected format potentially as untyped later if needed.
        error("Unknown module definition format: $line")
    end
end

function parse_input(filename::String)
    modules = Dict{String,AbstractModule}()
    lines = readlines(filename)

    # First pass: create all defined modules
    for line in lines
        module_obj = parse_line(line)
        modules[module_obj.name] = module_obj
    end

    # Second pass: populate conjunction inputs and handle undefined output modules
    all_module_names = Set(keys(modules))
    input_map = Dict{String,Vector{String}}() # module_name -> [input_module_names...]

    for (name, mod) in modules
        for output_name in mod.outputs
            # Keep track of who sends to whom
             if !haskey(input_map, output_name)
                 input_map[output_name] = []
             end
             push!(input_map[output_name], name)

            # If an output module wasn't defined on the left, add it as Untyped
            if !(output_name in all_module_names)
                # This handles cases like the 'output' module in examples
                # or the implicit 'rx' module in Part 2.
                modules[output_name] = UntypedModule(output_name, String[])
                push!(all_module_names, output_name) # Add to known names
            end
        end
    end

    # Initialize conjunction module inputs based on the input map
    for (name, mod) in modules
        if mod isa ConjunctionModule
            if haskey(input_map, name)
                for input_name in input_map[name]
                    # Default memory state is low (false)
                    mod.inputs[input_name] = false
                end
            end
             # If a conjunction module has no inputs defined in the file,
             # its inputs dict remains empty, which is technically correct
             # (it will always output high pulse as per the rule, as `all(values)` on empty is true)
        end
    end

    return modules
end

# --- Simulation Logic ---

# Represents a pulse being sent
struct Pulse
    source::String
    destination::String
    type::Bool # false = low, true = high
end

# Processes a pulse for a specific module, updates state, and returns outgoing pulse type (or nothing)
function process_pulse!(mod::FlipFlopModule, source::String, pulse_type::Bool)::Union{Bool,Nothing}
    if pulse_type # High pulse - ignored
        return nothing
    else # Low pulse - flip state and send pulse
        mod.state = !mod.state
        return mod.state # Send high if now on (true), low if now off (false)
    end
end

function process_pulse!(mod::ConjunctionModule, source::String, pulse_type::Bool)::Union{Bool,Nothing}
    # Update memory for the source input
    mod.inputs[source] = pulse_type
    # Check if all inputs are high
    all_high = true
    for val in values(mod.inputs)
        if !val
            all_high = false
            break
        end
    end
    # Send low if all high, otherwise send high
    return !all_high
end

function process_pulse!(mod::BroadcastModule, source::String, pulse_type::Bool)::Union{Bool,Nothing}
    # Simply relays the pulse
    return pulse_type
end

function process_pulse!(mod::UntypedModule, source::String, pulse_type::Bool)::Union{Bool,Nothing}
    # Does not process or send pulses
    return nothing
end


# Simulates one button press and returns counts of low and high pulses sent
function simulate_button_press!(modules::Dict{String,AbstractModule})
    low_pulse_count = 0
    high_pulse_count = 0
    
    # Use Julia's standard Vector as a queue
    pulse_queue = Vector{Pulse}()

    # Initial button press sends a low pulse to broadcaster
    push!(pulse_queue, Pulse("button", "broadcaster", false))
    low_pulse_count += 1

    while !isempty(pulse_queue)
        current_pulse = popfirst!(pulse_queue)
        
        dest_name = current_pulse.destination
        
        # Check if the destination module exists
        if !haskey(modules, dest_name)
            # Destination is not a defined module (e.g., 'output', 'rx'), ignore pulse
            continue
        end

        destination_module = modules[dest_name]

        # Process the pulse and get the type of pulse to send (if any)
        outgoing_pulse_type = process_pulse!(destination_module, current_pulse.source, current_pulse.type)

        if !isnothing(outgoing_pulse_type)
             # outgoing_pulse_type is guaranteed to be Bool here
             outgoing_pulse_bool = outgoing_pulse_type::Bool

             # Send the outgoing pulse to all destination outputs
             for output_name in destination_module.outputs
                 push!(pulse_queue, Pulse(dest_name, output_name, outgoing_pulse_bool))
                 if outgoing_pulse_bool # High pulse
                     high_pulse_count += 1
                 else # Low pulse
                     low_pulse_count += 1
                 end
             end
        end
    end

    return low_pulse_count, high_pulse_count
end

# --- Main Execution ---

function main()
    modules = parse_input("input.txt")

    total_low_pulses = 0
    total_high_pulses = 0

    for _ in 1:1000
        low_count, high_count = simulate_button_press!(modules)
        total_low_pulses += low_count
        total_high_pulses += high_count
    end

    result = total_low_pulses * total_high_pulses
    println(result)
end

# Run the main function
main()
