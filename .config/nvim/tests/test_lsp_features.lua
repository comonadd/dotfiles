-- Test LSP features and keymaps
print("\n=== Testing LSP Features ===\n")

-- Create a test TypeScript file
local test_file = "/tmp/nvim_test_features.ts"
vim.cmd('edit ' .. test_file)
vim.api.nvim_buf_set_lines(0, 0, -1, false, {
  'function add(a: number, b: number): number {',
  '  return a + b;',
  '}',
  '',
  'const result = add(1, 2);',
})

-- Wait for LSP to attach
vim.wait(1000)

local clients = vim.lsp.get_clients({ bufnr = 0 })
if #clients > 0 then
  print("✓ LSP attached for feature testing")

  -- Test hover capability
  vim.api.nvim_win_set_cursor(0, {1, 9}) -- Position on 'add'
  local has_hover = vim.lsp.get_clients({bufnr = 0})[1].server_capabilities.hoverProvider
  if has_hover then
    print("✓ Hover capability available")
  else
    print("✗ Hover capability NOT available")
  end

  -- Test goto definition capability
  local has_definition = vim.lsp.get_clients({bufnr = 0})[1].server_capabilities.definitionProvider
  if has_definition then
    print("✓ Go to definition capability available")
  else
    print("✗ Go to definition capability NOT available")
  end

  -- Test rename capability
  local has_rename = vim.lsp.get_clients({bufnr = 0})[1].server_capabilities.renameProvider
  if has_rename then
    print("✓ Rename capability available")
  else
    print("✗ Rename capability NOT available")
  end

  -- Test code action capability
  local has_code_action = vim.lsp.get_clients({bufnr = 0})[1].server_capabilities.codeActionProvider
  if has_code_action then
    print("✓ Code action capability available")
  else
    print("✗ Code action capability NOT available")
  end

  -- Test references capability
  local has_references = vim.lsp.get_clients({bufnr = 0})[1].server_capabilities.referencesProvider
  if has_references then
    print("✓ Find references capability available")
  else
    print("✗ Find references capability NOT available")
  end

else
  print("✗ LSP did NOT attach")
end

print("\n=================================\n")
