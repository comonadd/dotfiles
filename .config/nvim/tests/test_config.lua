-- Test script to verify Neovim configuration
local results = {}

-- Test 1: Check if node_host_prog is set
if vim.g.node_host_prog then
  table.insert(results, "✓ node_host_prog is set: " .. vim.g.node_host_prog)
else
  table.insert(results, "✗ node_host_prog is NOT set")
end

-- Test 2: Check if node_host_prog is executable
if vim.g.node_host_prog and vim.fn.executable(vim.g.node_host_prog) == 1 then
  table.insert(results, "✓ node_host_prog is executable")
else
  table.insert(results, "✗ node_host_prog is NOT executable")
end

-- Test 3: Check PATH
local path = vim.env.PATH or ""
if string.match(path, "fnm") then
  table.insert(results, "✓ PATH contains fnm")
else
  table.insert(results, "✗ PATH does NOT contain fnm")
end

-- Test 4: Check if node is in PATH
local node_path = vim.fn.exepath("node")
if node_path ~= "" then
  table.insert(results, "✓ node found in PATH: " .. node_path)
else
  table.insert(results, "✗ node NOT found in PATH")
end

-- Test 5: Check if tsserver is in PATH
local tsserver_path = vim.fn.exepath("tsserver")
if tsserver_path ~= "" then
  table.insert(results, "✓ tsserver found in PATH: " .. tsserver_path)
else
  table.insert(results, "✗ tsserver NOT found in PATH")
end

-- Print results
print("\n=== Neovim Configuration Test Results ===\n")
for _, result in ipairs(results) do
  print(result)
end
print("\n=========================================\n")
