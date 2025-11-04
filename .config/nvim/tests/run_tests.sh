#!/usr/bin/env bash

# Neovim Test Runner
# Runs all Neovim configuration tests

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo -e "${BLUE}╔═══════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   Neovim Configuration Test Suite            ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════╝${NC}"
echo ""

# Track test results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to run a test
run_test() {
    local test_name=$1
    local test_file=$2

    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    echo -e "${BLUE}Running: ${test_name}${NC}"
    echo "----------------------------------------"

    # Run the test and capture output
    if nvim --headless -c "luafile ${test_file}" -c "qa!" 2>&1; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
        echo -e "${GREEN}✓ ${test_name} completed${NC}\n"
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
        echo -e "${RED}✗ ${test_name} failed${NC}\n"
    fi
}

# Run all tests
run_test "Configuration Test" "${SCRIPT_DIR}/test_config.lua"
run_test "Plugin Loading Test" "${SCRIPT_DIR}/test_plugins.lua"
run_test "TypeScript LSP Test" "${SCRIPT_DIR}/test_typescript_lsp.lua"
run_test "Python LSP Test" "${SCRIPT_DIR}/test_python_lsp.lua"
run_test "Code Completion Test" "${SCRIPT_DIR}/test_completion.lua"
run_test "LSP Features Test" "${SCRIPT_DIR}/test_lsp_features.lua"

# Print summary
echo ""
echo -e "${BLUE}╔═══════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   Test Summary                                ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════════════╝${NC}"
echo ""
echo -e "Total Tests:  ${TOTAL_TESTS}"
echo -e "${GREEN}Passed:       ${PASSED_TESTS}${NC}"

if [ $FAILED_TESTS -gt 0 ]; then
    echo -e "${RED}Failed:       ${FAILED_TESTS}${NC}"
    echo ""
    exit 1
else
    echo ""
    echo -e "${GREEN}All tests passed! ✓${NC}"
    exit 0
fi
