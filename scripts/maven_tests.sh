#!/bin/bash

# Clean up any previous test artifacts
rm -f /tmp/mvn_output.log /tmp/failed_tests.txt

# Use 'script' to capture output while preserving color
script -q -c "mvn clean verify -Pcomponent-tests -Dotel.java.global-autoconfigure.enabled=false" /tmp/mvn_output.log
maven_exit_code=$?

# Display the output in real-time
cat /tmp/mvn_output.log

# Check for failures in the output
if grep -q "Failures:" /tmp/mvn_output.log; then
    echo "Checking for failed or errors..."

    # Extract failed tests and save to a file
    grep "Time elapsed:" /tmp/mvn_output.log | awk '{print $2}' | grep -oE '[^.]+Test\.[^.]+' | sed 's/\./#/g' > /tmp/failed_tests.txt

fi

# Exit with the same code as Maven to properly indicate success/failure
exit $maven_exit_code
