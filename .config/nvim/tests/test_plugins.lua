-- Test plugin loading
print("\n=== Testing Plugin Loading ===\n")

-- Wait for lazy.nvim to finish loading
vim.wait(2000)

-- Check if lazy.nvim is loaded
local lazy_ok, lazy = pcall(require, "lazy")
if lazy_ok then
  print("✓ lazy.nvim plugin manager loaded")

  -- Get all plugins
  local plugins = lazy.plugins()
  local plugin_count = 0
  for _ in pairs(plugins) do
    plugin_count = plugin_count + 1
  end
  print("  - Total plugins: " .. plugin_count)
else
  print("✗ lazy.nvim NOT loaded")
end

-- Test key plugins
local critical_plugins = {
  "nvim-lspconfig",
  "nvim-cmp",
  "nvim-treesitter",
  "typescript-tools.nvim",
  "gitsigns.nvim",
  "telescope.nvim",
  "ale",
}

print("\nChecking critical plugins:")
for _, plugin_name in ipairs(critical_plugins) do
  local found = false
  if lazy_ok then
    local plugins = lazy.plugins()
    for _, p in pairs(plugins) do
      if p.name == plugin_name or p[1]:find(plugin_name) then
        found = true
        break
      end
    end
  end

  if found then
    print("  ✓ " .. plugin_name)
  else
    print("  ✗ " .. plugin_name .. " NOT found")
  end
end

-- Check if nvim-cmp is functional
local cmp_ok, cmp = pcall(require, "cmp")
if cmp_ok then
  print("\n✓ nvim-cmp is functional")
  local sources = cmp.get_config().sources
  if sources and #sources > 0 then
    print("  - Completion sources configured: " .. #sources)
  end
else
  print("\n✗ nvim-cmp NOT functional")
end

-- Check if treesitter is functional
local ts_ok, ts = pcall(require, "nvim-treesitter.configs")
if ts_ok then
  print("\n✓ nvim-treesitter is functional")
else
  print("\n✗ nvim-treesitter NOT functional")
end

-- Check if ALE is loaded
if vim.g.ale_fixers then
  print("\n✓ ALE is loaded and configured")
  local fixer_count = 0
  for _ in pairs(vim.g.ale_fixers) do
    fixer_count = fixer_count + 1
  end
  print("  - Filetypes with fixers: " .. fixer_count)
else
  print("\n✗ ALE NOT configured")
end

print("\n=================================\n")
