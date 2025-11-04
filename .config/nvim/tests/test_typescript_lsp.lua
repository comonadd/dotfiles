-- Test TypeScript LSP
print("\n=== Testing TypeScript LSP ===\n")

-- Create test file in temp directory
local test_file = "/tmp/nvim_test_ts.ts"
local file = io.open(test_file, "w")
if file then
  file:write([[
// Test TypeScript file
function greet(name: string): string {
  return `Hello, ${name}!`;
}

const result = greet("World");
console.log(result);

// This should have a type error
const num: number = "not a number";
]])
  file:close()
end

-- Open the TypeScript file
vim.cmd('edit ' .. test_file)

-- Wait for LSP to attach (with timeout)
local max_wait = 50 -- 5 seconds
local wait_count = 0
while wait_count < max_wait do
  local clients = vim.lsp.get_clients({ bufnr = 0 })
  if #clients > 0 then
    print("✓ LSP client attached after " .. (wait_count * 0.1) .. " seconds")
    for _, client in ipairs(clients) do
      print("  - Client name: " .. client.name)
      print("  - Client ID: " .. client.id)
    end
    break
  end
  vim.wait(100) -- Wait 100ms
  wait_count = wait_count + 1
end

if wait_count >= max_wait then
  print("✗ LSP client did NOT attach within timeout")
end

-- Wait a bit more for diagnostics
vim.wait(2000)

-- Check diagnostics
local diagnostics = vim.diagnostic.get(0)
print("\n✓ Found " .. #diagnostics .. " diagnostic(s)")
if #diagnostics > 0 then
  print("Sample diagnostics:")
  for i, diag in ipairs(diagnostics) do
    if i <= 2 then -- Show first 2
      print("  - Line " .. (diag.lnum + 1) .. ": " .. diag.message)
    end
  end
end

print("\n=================================\n")
