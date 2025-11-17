-- Debug Adapter Protocol (DAP) for debugging Python tests
return {
  "mfussenegger/nvim-dap",
  dependencies = {
    "mfussenegger/nvim-dap-python",
    -- Optional but highly recommended UI plugins
    "rcarriga/nvim-dap-ui",
    "theHamsta/nvim-dap-virtual-text",
  },
  config = function()
    local dap = require("dap")
    local dap_python = require("dap-python")

    -- Setup Python debugging with project's virtual environment
    local function get_python_path()
      local cwd = vim.fn.getcwd()
      local venv_python = cwd .. "/.venv/bin/python"
      if vim.fn.executable(venv_python) == 1 then
        return venv_python
      end
      return "python3"
    end

    dap_python.setup(get_python_path())

    -- Configure pytest debugging
    table.insert(dap.configurations.python, {
      type = "python",
      request = "launch",
      name = "Pytest: Current File",
      module = "pytest",
      args = function()
        return { vim.fn.expand("%"), "-sv", "--no-cov" }
      end,
      console = "integratedTerminal",
      justMyCode = false,
    })

    table.insert(dap.configurations.python, {
      type = "python",
      request = "launch",
      name = "Pytest: Current Test",
      module = "pytest",
      args = function()
        -- Get the nearest test function name
        local line = vim.fn.line(".")
        local test_line = vim.fn.search("^def test_", "bn")
        local test_name = vim.fn.getline(test_line):match("def (test_[^(]+)")

        if test_name then
          return { vim.fn.expand("%") .. "::" .. test_name, "-sv", "--no-cov" }
        else
          return { vim.fn.expand("%"), "-sv", "--no-cov" }
        end
      end,
      console = "integratedTerminal",
      justMyCode = false,
    })

    -- DAP UI setup (optional but recommended)
    local has_dapui, dapui = pcall(require, "dapui")
    if has_dapui then
      dapui.setup({
        layouts = {
          {
            elements = {
              { id = "scopes", size = 0.25 },
              { id = "breakpoints", size = 0.25 },
              { id = "stacks", size = 0.25 },
              { id = "watches", size = 0.25 },
            },
            size = 40,
            position = "left",
          },
          {
            elements = {
              { id = "repl", size = 0.5 },
              { id = "console", size = 0.5 },
            },
            size = 10,
            position = "bottom",
          },
        },
      })

      -- Auto-open/close DAP UI
      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open()
      end
      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close()
      end
      dap.listeners.before.event_exited["dapui_config"] = function()
        dapui.close()
      end
    end

    -- Virtual text setup (shows variable values inline)
    local has_virtual_text, virtual_text = pcall(require, "nvim-dap-virtual-text")
    if has_virtual_text then
      virtual_text.setup({
        enabled = true,
        enabled_commands = true,
        highlight_changed_variables = true,
        highlight_new_as_changed = false,
        show_stop_reason = true,
        commented = false,
      })
    end

    -- DAP signs in sign column
    vim.fn.sign_define("DapBreakpoint", { text = "", texthl = "DiagnosticError", linehl = "", numhl = "" })
    vim.fn.sign_define("DapBreakpointCondition", { text = "", texthl = "DiagnosticWarn", linehl = "", numhl = "" })
    vim.fn.sign_define("DapLogPoint", { text = "", texthl = "DiagnosticInfo", linehl = "", numhl = "" })
    vim.fn.sign_define("DapStopped", { text = "", texthl = "DiagnosticHint", linehl = "Visual", numhl = "" })
    vim.fn.sign_define("DapBreakpointRejected", { text = "", texthl = "DiagnosticError", linehl = "", numhl = "" })
  end,
  keys = {
    -- Breakpoints
    { "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "Debug: Toggle Breakpoint" },
    { "<leader>dB", function() require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: ")) end, desc = "Debug: Conditional Breakpoint" },
    { "<leader>dL", function() require("dap").set_breakpoint(nil, nil, vim.fn.input("Log message: ")) end, desc = "Debug: Log Point" },

    -- Debugging controls
    { "<leader>dc", function() require("dap").continue() end, desc = "Debug: Continue" },
    { "<leader>dC", function() require("dap").run_to_cursor() end, desc = "Debug: Run to Cursor" },
    { "<leader>di", function() require("dap").step_into() end, desc = "Debug: Step Into" },
    { "<leader>do", function() require("dap").step_over() end, desc = "Debug: Step Over" },
    { "<leader>dO", function() require("dap").step_out() end, desc = "Debug: Step Out" },
    { "<leader>dq", function() require("dap").terminate() end, desc = "Debug: Terminate" },
    { "<leader>dr", function() require("dap").restart() end, desc = "Debug: Restart" },

    -- Python-specific
    { "<leader>dt", function() require("dap-python").test_method() end, desc = "Debug: Test Method" },
    { "<leader>dT", function() require("dap-python").test_class() end, desc = "Debug: Test Class" },

    -- REPL
    { "<leader>dR", function() require("dap").repl.toggle() end, desc = "Debug: Toggle REPL" },
    { "<leader>dl", function() require("dap").run_last() end, desc = "Debug: Run Last" },

    -- UI (if nvim-dap-ui is installed)
    { "<leader>du", function() require("dapui").toggle() end, desc = "Debug: Toggle UI" },
    { "<leader>de", function() require("dapui").eval() end, desc = "Debug: Eval", mode = { "n", "v" } },
  },
}
