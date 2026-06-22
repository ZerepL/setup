#!/bin/bash

ORIGINAL_DISPLAY="$DISPLAY"
export DISPLAY=:0.0

# Function to parse time argument and convert to seconds
parse_time() {
    local time_arg="$1"
    local duration=0
    
    if [[ $time_arg =~ ^([0-9]+)([smh])$ ]]; then
        local number="${BASH_REMATCH[1]}"
        local unit="${BASH_REMATCH[2]}"
        
        case $unit in
            s) duration=$number ;;
            m) duration=$((number * 60)) ;;
            h) duration=$((number * 3600)) ;;
        esac
    else
        echo "Invalid time format. Use: [number][s|m|h] (e.g., 30s, 5m, 2h)"
        exit 1
    fi
    
    echo $duration
}

cleanup() {
    export DISPLAY="$ORIGINAL_DISPLAY"
    echo -e "\nWake script stopped."
    exit
}

# Trap Ctrl+C and other exit signals
trap cleanup INT TERM

# Check if time argument is provided
if [ $# -eq 1 ]; then
    # Time-limited mode
    duration=$(parse_time "$1")
    echo "Running wake script for $1 ($duration seconds)..."
    
    end_time=$(($(date +%s) + duration))
    
    while [ $(date +%s) -lt $end_time ]; do
        xdotool mousemove_relative -- 1 0
        sleep 10
        xdotool mousemove_relative -- -1 0
        sleep 10
    done
    
    echo "Wake script finished after $1."
    cleanup
elif [ $# -eq 0 ]; then
    # Infinite mode (original behavior)
    echo "Running wake script indefinitely... (Press Ctrl+C to stop)"
    
    while true; do
        xdotool mousemove_relative -- 1 0
        sleep 10
        xdotool mousemove_relative -- -1 0
        sleep 10
    done
else
    echo "Usage: $0 [time]"
    echo "  time: Optional duration (e.g., 30s, 5m, 2h)"
    echo "  If no time is specified, runs indefinitely until stopped"
    exit 1
fi
