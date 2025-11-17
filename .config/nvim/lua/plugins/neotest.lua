-- Neotest: Modern test runner with visual feedback and watch mode
return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-neotest/nvim-nio",
    "antoinemadec/FixCursorHold.nvim",
    "nvim-treesitter/nvim-treesitter",
    "nvim-neotest/neotest-python",
  },
  config = function()
    require("neotest").setup({
      adapters = {
        require("neotest-python")({
          -- Use project's virtual environment
          python = function()
            local cwd = vim.fn.getcwd()
            local venv_python = cwd .. "/.venv/bin/python"
            if vim.fn.executable(venv_python) == 1 then
              return venv_python
            end
            return "python3"
          end,
          -- Configure pytest
          runner = "pytest",
          args = { "--log-level", "DEBUG", "--quiet" },
          -- Enable DAP debugging support
          dap = {
            justMyCode = false,
            -- Add --no-cov when debugging (coverage breaks debugging)
            console = "integratedTerminal",
          },
        }),
      },
      -- Display icons in sign column
      icons = {
        running = "",
        passed = "",
        failed = "",
        skipped = "",
        unknown = "",
      },
      -- Floating window for output
      output = {
        enabled = true,
        open_on_run = false,
      },
      -- Output panel shows history of all test runs
      output_panel = {
        enabled = true,
        open = "botright split | resize 15",
      },
      -- Integrate with Trouble.nvim for quickfix display
      quickfix = {
        enabled = true,
        open = function()
          if pcall(require, "trouble") then
            require("trouble").open({ mode = "quickfix", focus = false })
          else
            vim.cmd("copen")
          end
        end,
      },
      -- Status line integration (optional)
      status = {
        enabled = true,
        virtual_text = false,
        signs = true,
      },
      -- Show summary window with test hierarchy
      summary = {
        enabled = true,
        animated = true,
        follow = true,
        expand_errors = true,
      },
    })
  end,
  keys = {
    -- Run tests
    { "<leader>tr", function() require("neotest").run.run() end, desc = "Test Nearest" },
    { "<leader>tt", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Test File" },
    { "<leader>tT", function() require("neotest").run.run(vim.uv.cwd()) end, desc = "Test All" },
    { "<leader>tl", function() require("neotest").run.run_last() end, desc = "Test Last" },
    { "<leader>tS", function() require("neotest").run.stop() end, desc = "Test Stop" },

    -- Display
    { "<leader>ts", function() require("neotest").summary.toggle() end, desc = "Test Summary" },
    { "<leader>to", function() require("neotest").output.open({ enter = true, auto_close = true }) end, desc = "Test Output" },
    { "<leader>tO", function() require("neotest").output_panel.toggle() end, desc = "Test Output Panel" },

    -- Watch mode (auto-run tests on file save)
    { "<leader>tw", function() require("neotest").watch.toggle(vim.fn.expand("%")) end, desc = "Test Watch" },

    -- Debug (requires nvim-dap)
    { "<leader>td", function() require("neotest").run.run({ strategy = "dap" }) end, desc = "Debug Test" },
  },
}
