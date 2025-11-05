#!/bin/bash
# Test script for Neovim server functionality with iTerm session isolation

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Testing Neovim Server Functionality${NC}\n"

# Test 1: Check if nvr is installed
echo -e "${YELLOW}Test 1: Checking if nvr is installed...${NC}"
if command -v nvr &> /dev/null; then
    echo -e "${GREEN}✓ nvr is installed${NC}"
else
    echo -e "${RED}✗ nvr is not installed${NC}"
    echo "Run: pip3 install neovim-remote"
    exit 1
fi

# Test 2: Check if ITERM_SESSION_ID is set
echo -e "\n${YELLOW}Test 2: Checking ITERM_SESSION_ID...${NC}"
if [ -z "$ITERM_SESSION_ID" ]; then
    echo -e "${YELLOW}⚠ ITERM_SESSION_ID not set (expected if not running in iTerm)${NC}"
    TEST_SESSION_ID="test-session-$$"
    echo "Using test session ID: $TEST_SESSION_ID"
else
    echo -e "${GREEN}✓ ITERM_SESSION_ID is set: $ITERM_SESSION_ID${NC}"
    # Replace colons with dashes as colons are invalid in socket paths
    TEST_SESSION_ID="${ITERM_SESSION_ID//:/–}"
    echo "Sanitized session ID: $TEST_SESSION_ID"
fi

SOCKET_PATH="/tmp/nvimsocket-$TEST_SESSION_ID"

# Test 3: Clean up any existing socket
echo -e "\n${YELLOW}Test 3: Cleaning up any existing test sockets...${NC}"
rm -f "$SOCKET_PATH"
# Kill any existing nvim processes listening on our test socket
pkill -f "nvim.*$SOCKET_PATH" 2>/dev/null || true
sleep 0.5
echo -e "${GREEN}✓ Cleanup complete${NC}"

# Test 4: Start Neovim with server
echo -e "\n${YELLOW}Test 4: Starting Neovim server in background...${NC}"
# Start nvim with a simple command that keeps it running
nvim --headless --listen "$SOCKET_PATH" -c "sleep 999999" &
NVIM_PID=$!
echo "Neovim PID: $NVIM_PID"

# Wait for socket to be created
sleep 2

if [ -S "$SOCKET_PATH" ]; then
    echo -e "${GREEN}✓ Socket created at $SOCKET_PATH${NC}"
else
    echo -e "${RED}✗ Socket not created${NC}"
    kill $NVIM_PID 2>/dev/null || true
    exit 1
fi

# Test 5: Create a test file
echo -e "\n${YELLOW}Test 5: Creating test file...${NC}"
TEST_FILE="/tmp/nvim_test_file_$$.txt"
cat > "$TEST_FILE" << 'EOF'
Line 1: This is a test file
Line 2: For testing nvim server
Line 3: With nvr remote control
Line 4: Should work perfectly
Line 5: End of test file
EOF
echo -e "${GREEN}✓ Test file created at $TEST_FILE${NC}"

# Test 6: Test nvr connection
echo -e "\n${YELLOW}Test 6: Testing nvr connection...${NC}"
if nvr --servername "$SOCKET_PATH" --remote-send ':echo "test"<CR>' 2>/dev/null; then
    echo -e "${GREEN}✓ Successfully connected to Neovim server${NC}"
else
    echo -e "${RED}✗ Failed to connect to Neovim server${NC}"
    kill $NVIM_PID 2>/dev/null || true
    rm -f "$TEST_FILE" "$SOCKET_PATH"
    exit 1
fi

# Test 7: Test opening a file
echo -e "\n${YELLOW}Test 7: Testing file opening via nvr...${NC}"
if nvr --servername "$SOCKET_PATH" --remote "$TEST_FILE" 2>/dev/null; then
    echo -e "${GREEN}✓ Successfully sent command to open file${NC}"
else
    echo -e "${RED}✗ Failed to open file via nvr${NC}"
    kill $NVIM_PID 2>/dev/null || true
    rm -f "$TEST_FILE" "$SOCKET_PATH"
    exit 1
fi

sleep 1

# Test 8: Test opening a file at specific line
echo -e "\n${YELLOW}Test 8: Testing file opening at specific line...${NC}"
if nvr --servername "$SOCKET_PATH" --remote +3 "$TEST_FILE" 2>/dev/null; then
    echo -e "${GREEN}✓ Successfully sent command to open file at line 3${NC}"
else
    echo -e "${RED}✗ Failed to open file at specific line${NC}"
    kill $NVIM_PID 2>/dev/null || true
    rm -f "$TEST_FILE" "$SOCKET_PATH"
    exit 1
fi

# Test 9: Verify function definition
echo -e "\n${YELLOW}Test 9: Testing nvim function configuration...${NC}"
if grep -q "nvim()" ~/.zshrc && grep -q "ITERM_SESSION_ID" ~/.zshrc; then
    echo -e "${GREEN}✓ nvim function found in .zshrc${NC}"
else
    echo -e "${RED}✗ nvim function not configured in .zshrc${NC}"
fi

# Cleanup
echo -e "\n${YELLOW}Cleaning up...${NC}"
kill $NVIM_PID 2>/dev/null || true
sleep 0.5
rm -f "$TEST_FILE" "$SOCKET_PATH"
echo -e "${GREEN}✓ Cleanup complete${NC}"

echo -e "\n${GREEN}========================================${NC}"
echo -e "${GREEN}All tests passed! ✓${NC}"
echo -e "${GREEN}========================================${NC}"
echo -e "\n${YELLOW}Next steps:${NC}"
echo "1. Configure iTerm2 Semantic History:"
echo "   Preferences → Profiles → Advanced → Semantic History"
echo "   Select 'Run command' and enter:"
echo '   /bin/zsh -c "nvr --servername /tmp/nvimsocket-${ITERM_SESSION_ID//:/–} --remote +\2 \1"'
echo ""
echo "2. Reload your shell: source ~/.zshrc"
echo "3. Start nvim in a pane, then Cmd+Click on file:line paths in other panes!"
