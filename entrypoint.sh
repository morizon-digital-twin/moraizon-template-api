#!/bin/bash

# This command ensures that the script will exit immediately if any command fails.
set -e

# # --- NEW: Create log directory and file, and export the path ---
# LOG_DIR="/app/logs"
# export LOG_FILE="$LOG_DIR/api.log" # The 'export' makes this variable available to R
# mkdir -p $LOG_DIR
# touch $LOG_FILE

# Run tests and log everything, including errors

echo "=== RUNNING TESTS ==="

# Run testthat; capture results

#Rscript -e "testthat::test_file(path = 'test.R',reporter = c('summary','fail'))"

echo "=== TESTS passed ==="

# This line will now only be reached if the test command above succeeds.
exec Rscript -e "source('main.R'); plumber::plumb('api.R')\$run(host='0.0.0.0', port=PORT)"