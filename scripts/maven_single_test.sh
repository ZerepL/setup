#!/bin/bash

cd ./component-tests || exit

# Check if the argument is provided
if [[ -z "$1" ]]; then
    echo "Usage: $0 <ClassName.MethodName>"
    exit 1
fi

# Replace the dot with a hash symbol
formatted_test=$(echo "$1" | sed 's/\./#/g')

# Construct and run the mvn command
test_command="mvn -Dtest=$formatted_test test"
echo "Running: $test_command"
eval "$test_command"
