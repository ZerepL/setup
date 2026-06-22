#!/bin/bash

# Navigate to the component-tests directory
cd ./component-tests || exit

# Check if the failed_tests.txt file exists
if [[ ! -f /tmp/failed_tests.txt ]]; then
    echo "No failed tests found. Please run the first script first."
    exit 1
fi

# Check if the failed_tests.txt file is empty
if [[ ! -s /tmp/failed_tests.txt ]]; then
    echo "The failed_tests.txt file is empty. No tests to run."
    exit 0
fi

# Construct the mvn command with all failed tests as a comma-separated list
test_command="mvn -Dtest="

# Read the failed tests, join them with commas
tests=$(paste -sd, /tmp/failed_tests.txt)

# Append tests to the command
test_command+="$tests test"

# Count total of failed tests before
initial_lines=$(wc -l /tmp/failed_tests.txt)

# Run the command
echo "Running failed tests..."
script -q -c "$test_command" /tmp/mvn_output.log

cat /tmp/mvn_output.log

grep "Time elapsed:" /tmp/mvn_output.log | awk '{print $2}' | grep -oE '[^.]+Test\.[^.]+' | sed 's/\./#/g' > /tmp/failed_tests.txt

# Count total of failed tests after
new_lines=$(wc -l /tmp/failed_tests.txt)

echo "Total previous errors: $initial_lines\nActual total errors: $new_lines"
