#!/bin/bash

# Validation Report Script for Route Planning Repository Server
# This script runs the complete validation workflow and generates a status report

# File paths for logs and reports
BUILD_LOG="/tmp/build.log"
SERVER_LOG="/tmp/server.log"
TESTS_LOG="/tmp/component_tests.log"
REPORT_FILE="/tmp/validation_report.txt"

# Timeout settings (in seconds)
BUILD_TIMEOUT=120    # 2 minutes for build
SERVER_TIMEOUT=30    # 30 seconds for server startup
TESTS_TIMEOUT=180    # 3 minutes for component tests

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to run command with timeout
run_with_timeout() {
    local timeout=$1
    local description=$2
    shift 2
    
    echo "Running: $description (timeout: ${timeout}s)"
    
    # Run command in background
    "$@" &
    local cmd_pid=$!
    
    # Wait for command with timeout
    local count=0
    while kill -0 $cmd_pid 2>/dev/null; do
        if [ $count -ge $timeout ]; then
            echo "⏰ TIMEOUT: $description exceeded ${timeout}s limit"
            kill -TERM $cmd_pid 2>/dev/null || true
            sleep 2
            kill -KILL $cmd_pid 2>/dev/null || true
            return 124  # Standard timeout exit code
        fi
        sleep 1
        ((count++))
    done
    
    # Get the actual exit code
    wait $cmd_pid
    return $?
}

# Initialize report file
init_report() {
    echo "=== VALIDATION REPORT - $(date) ===" > $REPORT_FILE
    echo "Project: Route Planning Repository Server" >> $REPORT_FILE
    echo "Location: $(pwd)" >> $REPORT_FILE
    echo "Timeouts: Build=${BUILD_TIMEOUT}s, Server=${SERVER_TIMEOUT}s, Tests=${TESTS_TIMEOUT}s" >> $REPORT_FILE
    echo "" >> $REPORT_FILE
}

# Build the project
run_build() {
    echo -e "${YELLOW}Building project...${NC}"
    echo "STEP 1: Building project..." >> $REPORT_FILE
    
    local start_time=$(date +%s)
    
    # Create a wrapper function for the build command
    build_command() {
        mvn clean install > $BUILD_LOG 2>&1
    }
    
    if run_with_timeout $BUILD_TIMEOUT "Maven build" build_command; then
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        echo -e "${GREEN}✅ BUILD: SUCCESS${NC}"
        echo "✅ BUILD: SUCCESS (${duration}s)" >> $REPORT_FILE
        return 0
    else
        local exit_code=$?
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        
        if [ $exit_code -eq 124 ]; then
            echo -e "${RED}❌ BUILD: TIMEOUT${NC}"
            echo "❌ BUILD: TIMEOUT after ${BUILD_TIMEOUT}s (${duration}s) - Check $BUILD_LOG" >> $REPORT_FILE
        else
            echo -e "${RED}❌ BUILD: FAILED${NC}"
            echo "❌ BUILD: FAILED (${duration}s) - Check $BUILD_LOG" >> $REPORT_FILE
        fi
        return 1
    fi
}

# Start the server
start_server() {
    echo -e "${YELLOW}Starting server...${NC}"
    echo "STEP 2: Starting server..." >> $REPORT_FILE
    
    local start_time=$(date +%s)
    
    # Kill any existing server processes
    pkill -f "spring-boot:run" 2>/dev/null || true
    sleep 2
    
    # Start server in background
    mvn spring-boot:run -Dspring-boot.run.jvmArguments="-Xmx2g -Xms1g" -f app/ > $SERVER_LOG 2>&1 &
    local server_pid=$!
    
    # Wait for server to start with timeout
    echo "Waiting for server to start (max ${SERVER_TIMEOUT}s)..."
    local count=0
    local server_started=false
    
    while [ $count -lt $SERVER_TIMEOUT ]; do
        # First check if the process is still alive
        if ! kill -0 $server_pid 2>/dev/null; then
            local end_time=$(date +%s)
            local duration=$((end_time - start_time))
            echo -e "${RED}❌ SERVER: PROCESS DIED DURING STARTUP${NC}"
            echo "❌ SERVER: PROCESS DIED DURING STARTUP (${duration}s) - Check $SERVER_LOG" >> $REPORT_FILE
            
            # Show last few lines of server log for debugging
            if [ -f $SERVER_LOG ]; then
                echo "Last 10 lines from server log:" >> $REPORT_FILE
                tail -10 $SERVER_LOG >> $REPORT_FILE 2>/dev/null || true
            fi
            return 1
        fi
        
        # Check for successful startup indicators in the log
        if [ -f $SERVER_LOG ]; then
            # Look for Spring Boot startup completion messages
            if grep -q "Started.*Application.*in.*seconds" $SERVER_LOG 2>/dev/null; then
                server_started=true
                break
            fi
            
            # Also check for common startup success patterns
            if grep -q "Tomcat started on port" $SERVER_LOG 2>/dev/null; then
                server_started=true
                break
            fi
            
            # Check for startup failure indicators
            if grep -q -E "(APPLICATION FAILED TO START|Error starting ApplicationContext|Exception in thread.*main)" $SERVER_LOG 2>/dev/null; then
                local end_time=$(date +%s)
                local duration=$((end_time - start_time))
                echo -e "${RED}❌ SERVER: STARTUP FAILED${NC}"
                echo "❌ SERVER: STARTUP FAILED (${duration}s) - Check $SERVER_LOG" >> $REPORT_FILE
                
                # Show error details from log
                echo "Error details from server log:" >> $REPORT_FILE
                grep -A 5 -B 5 -E "(APPLICATION FAILED TO START|Error starting ApplicationContext|Exception in thread.*main)" $SERVER_LOG >> $REPORT_FILE 2>/dev/null || true
                
                kill $server_pid 2>/dev/null || true
                return 1
            fi
        fi
        
        # After 20 seconds, try a health check if no startup message found
        if [ $count -ge 20 ] && [ "$server_started" = false ]; then
            # Try to detect if server is listening on common ports
            if netstat -tln 2>/dev/null | grep -q ":8080.*LISTEN" || \
               netstat -tln 2>/dev/null | grep -q ":8443.*LISTEN" || \
               ss -tln 2>/dev/null | grep -q ":8080.*LISTEN" || \
               ss -tln 2>/dev/null | grep -q ":8443.*LISTEN"; then
                echo "Server appears to be listening on port 8080/8443"
                server_started=true
                break
            fi
            
            # Try a simple HTTP health check (if curl is available)
            if command -v curl >/dev/null 2>&1; then
                if curl -s --connect-timeout 2 --max-time 5 http://localhost:8080/actuator/health >/dev/null 2>&1 || \
                   curl -s --connect-timeout 2 --max-time 5 http://localhost:8080/health >/dev/null 2>&1 || \
                   curl -s --connect-timeout 2 --max-time 5 http://localhost:8080/ >/dev/null 2>&1; then
                    echo "Server responding to HTTP requests"
                    server_started=true
                    break
                fi
            fi
        fi
        
        sleep 1
        ((count++))
    done
    
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    # Final check after timeout
    if [ $count -ge $SERVER_TIMEOUT ]; then
        if kill -0 $server_pid 2>/dev/null; then
            # Process is alive but we didn't see startup completion
            echo -e "${RED}❌ SERVER: STARTUP TIMEOUT${NC}"
            echo "❌ SERVER: STARTUP TIMEOUT after ${SERVER_TIMEOUT}s - Check $SERVER_LOG" >> $REPORT_FILE
            echo "Process is still running but startup not confirmed" >> $REPORT_FILE
            
            # Add some diagnostic info
            if [ -f $SERVER_LOG ]; then
                echo "Last 15 lines from server log:" >> $REPORT_FILE
                tail -15 $SERVER_LOG >> $REPORT_FILE 2>/dev/null || true
            fi
            
            kill $server_pid 2>/dev/null || true
        else
            echo -e "${RED}❌ SERVER: PROCESS DIED${NC}"
            echo "❌ SERVER: PROCESS DIED during startup (${duration}s) - Check $SERVER_LOG" >> $REPORT_FILE
        fi
        return 1
    elif [ "$server_started" = true ] && kill -0 $server_pid 2>/dev/null; then
        echo -e "${GREEN}✅ SERVER: STARTED${NC}"
        echo "✅ SERVER: STARTED (${duration}s) - PID: $server_pid" >> $REPORT_FILE
        echo $server_pid > /tmp/server.pid
        return 0
    else
        echo -e "${RED}❌ SERVER: FAILED TO START${NC}"
        echo "❌ SERVER: FAILED TO START (${duration}s) - Check $SERVER_LOG" >> $REPORT_FILE
        return 1
    fi
}

# Run component tests
run_component_tests() {
    echo -e "${YELLOW}Running component tests...${NC}"
    echo "STEP 3: Running component tests..." >> $REPORT_FILE
    
    # Clean up any previous test artifacts to avoid false positives
    rm -f /tmp/failed_tests.txt /tmp/mvn_output.log
    
    local start_time=$(date +%s)
    
    # Create a wrapper function for the test command
    test_command() {
        $UTILS/scripts/maven_tests.sh > $TESTS_LOG 2>&1
    }
    
    local test_result
    if run_with_timeout $TESTS_TIMEOUT "Component tests" test_command; then
        test_result=0
    else
        test_result=$?
    fi
    
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    local minutes=$((duration / 60))
    local seconds=$((duration % 60))
    
    # Count test results from log
    local tests_run=$(grep -c "Tests run:" $TESTS_LOG 2>/dev/null || echo "Unknown")
    local failures=$(grep "Failures:" $TESTS_LOG | tail -1 | sed 's/.*Failures: \([0-9]*\).*/\1/' 2>/dev/null || echo "0")
    local errors=$(grep "Errors:" $TESTS_LOG | tail -1 | sed 's/.*Errors: \([0-9]*\).*/\1/' 2>/dev/null || echo "0")
    
    # Check for test failures in multiple ways:
    local has_failures=false
    
    if [ $test_result -eq 124 ]; then
        # Timeout occurred
        echo -e "${RED}❌ COMPONENT TESTS: TIMEOUT${NC}"
        echo "❌ COMPONENT TESTS: TIMEOUT after ${TESTS_TIMEOUT}s (${minutes}m ${seconds}s) - Check $TESTS_LOG" >> $REPORT_FILE
        return 1
    elif [ $test_result -ne 0 ]; then
        has_failures=true
    fi
    
    if grep -q "BUILD FAILURE" $TESTS_LOG 2>/dev/null; then
        has_failures=true
    fi
    
    if [ "$failures" != "0" ] && [ "$failures" != "" ] && [ "$failures" != "Unknown" ]; then
        has_failures=true
    fi
    
    if [ "$errors" != "0" ] && [ "$errors" != "" ] && [ "$errors" != "Unknown" ]; then
        has_failures=true
    fi
    
    # Only check for failed tests file if it was created during this run
    if [ -f /tmp/failed_tests.txt ] && [ -s /tmp/failed_tests.txt ]; then
        has_failures=true
    fi
    
    if [ "$has_failures" = true ]; then
        echo -e "${RED}❌ COMPONENT TESTS: FAILED${NC}"
        echo "❌ COMPONENT TESTS: FAILED (${minutes}m ${seconds}s) - Check $TESTS_LOG" >> $REPORT_FILE
        echo "   Tests Run: $tests_run, Failures: $failures, Errors: $errors" >> $REPORT_FILE
        echo "   Exit Code: $test_result" >> $REPORT_FILE
        
        # Add failed tests details if available
        if [ -f /tmp/failed_tests.txt ] && [ -s /tmp/failed_tests.txt ]; then
            echo "   Failed tests:" >> $REPORT_FILE
            while IFS= read -r failed_test; do
                echo "     - $failed_test" >> $REPORT_FILE
            done < /tmp/failed_tests.txt
        fi
        
        return 1
    else
        echo -e "${GREEN}✅ COMPONENT TESTS: PASSED${NC}"
        echo "✅ COMPONENT TESTS: PASSED (${minutes}m ${seconds}s)" >> $REPORT_FILE
        echo "   Tests Run: $tests_run, Failures: $failures, Errors: $errors" >> $REPORT_FILE
        return 0
    fi
}

# Generate final report summary
generate_final_report() {
    echo "" >> $REPORT_FILE
    echo "=== SUMMARY ===" >> $REPORT_FILE
    echo "Report generated at: $(date)" >> $REPORT_FILE
    echo "Log files available at:" >> $REPORT_FILE
    echo "  Build log: $BUILD_LOG" >> $REPORT_FILE
    echo "  Server log: $SERVER_LOG" >> $REPORT_FILE
    echo "  Tests log: $TESTS_LOG" >> $REPORT_FILE
    echo "" >> $REPORT_FILE
    
    # Display report to console
    echo -e "\n${YELLOW}=== VALIDATION SUMMARY ===${NC}"
    cat $REPORT_FILE | grep -E "(✅|❌)"
    echo -e "\nFull report available at: ${YELLOW}$REPORT_FILE${NC}"
}

# Cleanup function
cleanup() {
    echo -e "\n${YELLOW}Cleaning up...${NC}"
    if [ -f /tmp/server.pid ]; then
        local server_pid=$(cat /tmp/server.pid)
        if kill -0 $server_pid 2>/dev/null; then
            kill $server_pid 2>/dev/null || true
            echo "Server process $server_pid terminated"
        fi
        rm -f /tmp/server.pid
    fi
    
    # Kill any remaining spring-boot processes
    pkill -f "spring-boot:run" 2>/dev/null || true
    
    # Kill any remaining maven processes that might be stuck
    pkill -f "maven" 2>/dev/null || true
    
    echo "Cleanup completed"
}

# Set trap for cleanup on exit and signals
trap cleanup EXIT INT TERM

# Main execution
main() {
    echo -e "${GREEN}=== Route Planning Repository Server - Validation Report ===${NC}"
    echo "Starting validation process with timeouts..."
    echo "⏰ Timeouts: Build=${BUILD_TIMEOUT}s, Server=${SERVER_TIMEOUT}s, Tests=${TESTS_TIMEOUT}s"
    
    init_report
    
    # Run build
    if ! run_build; then
        echo -e "${RED}Build failed. Stopping validation process.${NC}"
        generate_final_report
        exit 1
    fi
    
    # Start server
    if ! start_server; then
        echo -e "${RED}Server failed to start. Stopping validation process.${NC}"
        generate_final_report
        exit 1
    fi
    
    # Run component tests
    if ! run_component_tests; then
        echo -e "${RED}Component tests failed.${NC}"
        generate_final_report
        exit 1
    fi
    
    # Generate final report
    generate_final_report
    
    echo -e "${GREEN}✅ All validation steps completed successfully!${NC}"
    echo -e "Ready to commit changes."
}

# Execute main function
main "$@"
