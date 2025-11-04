-- Test code completion
print("\n=== Testing Code Completion ===\n")

-- Test with TypeScript
local ts_file = "/tmp/nvim_test_completion.ts"
vim.cmd('edit ' .. ts_file)
vim.api.nvim_buf_set_lines(0, 0, -1, false, {
  'const arr = [1, 2, 3];',
  'arr.'
})

-- Wait for LSP to attach
vim.wait(500)
local clients = vim.lsp.get_clients({ bufnr = 0 })
if #clients > 0 then
  print("✓ TypeScript LSP attached for completion test")
else
  print("✗ TypeScript LSP did NOT attach")
end

-- Move cursor to position for completion
vim.api.nvim_win_set_cursor(0, {2, 4})

-- Wait for LSP to be ready
vim.wait(1000)

-- Request completion
local params = vim.lsp.util.make_position_params()
local result = vim.lsp.buf_request_sync(0, 'textDocument/completion', params, 2000)

if result and next(result) then
  print("✓ Received completion results from TypeScript LSP")
  local items = 0
  for client_id, res in pairs(result) do
    if res.result and res.result.items then
      items = #res.result.items
    elseif res.result and type(res.result) == 'table' then
      items = #res.result
    end
  end
  print("  - Found " .. items .. " completion items")
else
  print("✗ No completion results received")
end

-- Test with Python
print("\n--- Testing Python Completion ---\n")
local py_file = "/tmp/nvim_test_completion.py"
vim.cmd('edit ' .. py_file)
vim.api.nvim_buf_set_lines(0, 0, -1, false, {
  'import os',
  'os.'
})

-- Wait for LSP to attach
vim.wait(500)
clients = vim.lsp.get_clients({ bufnr = 0 })
if #clients > 0 then
  print("✓ Python LSP attached for completion test")
else
  print("✗ Python LSP did NOT attach")
end

-- Move cursor to position for completion
vim.api.nvim_win_set_cursor(0, {2, 3})

-- Wait for LSP to be ready
vim.wait(1000)

-- Request completion
params = vim.lsp.util.make_position_params()
result = vim.lsp.buf_request_sync(0, 'textDocument/completion', params, 2000)

if result and next(result) then
  print("✓ Received completion results from Python LSP")
  local items = 0
  for client_id, res in pairs(result) do
    if res.result and res.result.items then
      items = #res.result.items
    elseif res.result and type(res.result) == 'table' then
      items = #res.result
    end
  end
  print("  - Found " .. items .. " completion items")
else
  print("✗ No completion results received")
end

print("\n=================================\n")
