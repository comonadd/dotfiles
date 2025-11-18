-- Plugin setup with lazy.nvim (modern plugin manager)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

-- Plugin specifications
local plugins = {
	-- Visual
	"RRethy/vim-illuminate",
	"nvim-tree/nvim-web-devicons",

	-- Treesitter - modern syntax highlighting
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = { "python", "typescript", "tsx", "javascript", "yaml", "json", "lua", "bash", "markdown" },
				highlight = { enable = true },
				indent = { enable = true },
			})
		end,
	},

	-- Aerial - symbol outline and navigation
	{
		"stevearc/aerial.nvim",
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"nvim-tree/nvim-web-devicons"
		},
		config = function()
			require("aerial").setup({
				-- Use both treesitter and LSP for symbol extraction
				backends = { "treesitter", "lsp", "markdown", "man" },
				-- Show symbol outline on the right side
				layout = {
					max_width = { 40, 0.2 },
					width = nil,
					min_width = 10,
					default_direction = "prefer_right",
				},
				-- Disable on startup, toggle manually
				attach_mode = "global",
				close_automatic_events = {},
				-- Filter symbols by kind
				filter_kind = {
					"Class",
					"Constructor",
					"Enum",
					"Function",
					"Interface",
					"Module",
					"Method",
					"Struct",
				},
				-- Icons for different symbol types
				icons = {
					Class = "󰌗 ",
					Constructor = " ",
					Enum = "󰕘 ",
					Function = "󰊕 ",
					Interface = "󰕘 ",
					Method = "󰆧 ",
					Module = " ",
					Struct = "󰌗 ",
				},
				-- Highlight symbol under cursor
				highlight_on_hover = true,
				-- Auto-update symbols while typing
				autojump = false,
				-- Performance: update delay
				update_delay = 100,
			})
		end,
	},

	-- Linting and formatting
	{
		"dense-analysis/ale",
		config = function()
			vim.g.ale_fixers = {
				python = { "ruff", "ruff_format" },
				javascript = { "prettier", "eslint" },
				typescript = { "prettier", "eslint" },
				typescriptreact = { "prettier", "eslint" },
				javascriptreact = { "prettier", "eslint" },
				json = { "prettier", "trim_whitespace", "remove_trailing_lines" },
				lua = { "stylua", "trim_whitespace", "remove_trailing_lines" },
				make = { "trim_whitespace", "remove_trailing_lines" },
				html = { "remove_trailing_lines", "html-beautify", "trim_whitespace" },
				yaml = { "remove_trailing_lines", "trim_whitespace", "prettier" },
				sh = { "remove_trailing_lines", "trim_whitespace", "shfmt" },
				dosini = { "trim_whitespace", "remove_trailing_lines" },
			}

			vim.g.ale_linters = {
				python = { "ruff" },
				yaml = { "yamllint", "actionlint" },
				sh = { "bashate" },
			}

			vim.g.ale_python_ruff_options = "--config=/Users/guzeev/Work/visible-web/backend/ruff.toml"
			vim.g.ale_python_autoflake_options = "--remove-all-unused-imports --remove-unused-variables --in-place"
			vim.g.ale_python_mypy_options = "--show-error-codes --no-error-summary"
			vim.g.ale_fix_on_save = 1
			vim.g.ale_lint_on_text_changed = 'never'
			vim.g.ale_lint_on_insert_leave = 0
		end,
	},

	-- Search
	"mileszs/ack.vim",

	-- FZF
	{
		"junegunn/fzf",
		dir = "~/.fzf",
		build = "./install --all",
	},
	"junegunn/fzf.vim",
	"eugen0329/vim-esearch",

	-- FZF-Lua - Fast LSP operations
	{
		"ibhagwan/fzf-lua",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("fzf-lua").setup({
				winopts = {
					height = 0.85,
					width = 0.80,
					preview = {
						default = "bat",
						layout = "flex",
						flip_columns = 120,
						-- Performance: delay preview rendering
						delay = 100,
					},
				},
				files = {
					git_icons = false,
					file_icons = true,
				},
				grep = {
					rg_opts = "--hidden --column --line-number --no-heading --color=always --smart-case --glob '!.git/' --glob '!node_modules/' --glob '!.venv/' --glob '!__pycache__/'",
				},
				lsp = {
					-- Performance: increase timeout for large projects
					async_or_timeout = 5000,
					-- Disable preview for workspace symbols (huge performance boost)
					symbols = {
						symbol_icons = {
							File = "󰈙",
							Module = "",
							Namespace = "󰌗",
							Package = "",
							Class = "󰌗",
							Method = "󰆧",
							Property = "",
							Field = "",
							Constructor = "",
							Enum = "󰕘",
							Interface = "󰕘",
							Function = "󰊕",
							Variable = "󰆧",
							Constant = "󰏿",
							String = "󰀬",
							Number = "󰎠",
							Boolean = "◩",
							Array = "󰅪",
							Object = "󰅩",
							Key = "󰌋",
							Null = "󰟢",
							EnumMember = "",
							Struct = "󰌗",
							Event = "",
							Operator = "󰆕",
							TypeParameter = "󰊄",
						},
					},
				},
			})
		end,
	},

	-- Universal ctags auto-generation and management
	{
		"ludovicchabant/vim-gutentags",
		enabled = false,
		config = function()
			-- Only generate tags for project roots
			vim.g.gutentags_project_root = {'.git', 'package.json', 'pyproject.toml', 'Cargo.toml'}

			-- Cache directory for tags (create if doesn't exist)
			local tags_cache_dir = vim.fn.stdpath('cache') .. '/tags'
			vim.fn.mkdir(tags_cache_dir, 'p')
			vim.g.gutentags_cache_dir = tags_cache_dir

			-- Exclude patterns
			vim.g.gutentags_ctags_exclude = {
				'.git', 'node_modules', '.venv', '__pycache__',
				'*.min.js', '*.lock', 'package-lock.json',
				'*.css', 'dist', 'build', '.tox',
				'*.egg-info', '.mypy_cache', '.pytest_cache',
				'htmlcov', 'migrations', '*.pyc'
			}

			-- Use universal-ctags options
			vim.g.gutentags_ctags_extra_args = {
				'--tag-relative=yes',
				'--fields=+ailmnS',
				'--extras=+q',
			}

			-- Add language-specific options
			vim.g.gutentags_ctags_extra_args_python = {
				'--python-kinds=-iv'  -- Exclude variables and imports
			}
		end,
	},

	-- Text manipulation
	"tpope/vim-abolish",
	"tpope/vim-eunuch",
	"tpope/vim-surround",

	-- Syntax highlighting
	"vim-python/python-syntax",
	"justinmk/vim-syntax-extra",
	"elzr/vim-json",
	"plasticboy/vim-markdown",
	"pboettch/vim-cmake-syntax",
	"evanleck/vim-svelte",
	"sheerun/vim-polyglot",
	"pineapplegiant/spaceduck",

	-- Recent files
	"yegappan/mru",

	-- Telescope
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release",
		config = function()
			require("telescope").setup({
				defaults = {
					file_ignore_patterns = {
						"%.lock",
						"node_modules/.*",
						"%.git/.*",
						"__pycache__/.*",
						"%.venv/.*",
						"venv/.*",
						"%.tox/.*",
						"%.pytest_cache/.*",
						"%.mypy_cache/.*",
						"htmlcov/.*",
						"%.egg%-info/.*",
						"dist/.*",
						"build/.*",
						"%.so$",
						"%.pyc$",
						"migrations/.*",
					},
					vimgrep_arguments = {
						"rg",
						"--color=never",
						"--no-heading",
						"--with-filename",
						"--line-number",
						"--column",
						"--smart-case",
						"--hidden",
						"--glob=!.git/",
						"--glob=!.venv/",
						"--glob=!node_modules/",
						"--glob=!__pycache__/",
					},
					initial_mode = "insert",
					sorting_strategy = "ascending",
					layout_config = {
						horizontal = {
							prompt_position = "top",
							preview_width = 0.55,
						},
					},
					cache_picker = {
						num_pickers = 10,
					},
					-- Performance optimizations
					path_display = { "smart" },  -- Smart path truncation
					dynamic_preview_title = true,
					preview = {
						timeout = 250,  -- Reduce preview timeout (ms)
						filesize_limit = 1,  -- Skip preview for files > 1MB
					},
					file_sorter = require('telescope.sorters').get_fzy_sorter,
					generic_sorter = require('telescope.sorters').get_fzy_sorter,
				},
				pickers = {
					lsp_dynamic_workspace_symbols = {
						sorter = require("telescope").extensions.fzf.native_fzf_sorter(),
						show_line = false,
						-- Performance: only search after typing 3+ characters (increased from 2)
						on_input_filter_cb = function(prompt)
							return #prompt > 2
						end,
						-- Performance: disable preview for workspace symbols (major speed boost)
						previewer = false,
						-- Performance: truncate long names
						fname_width = 40,
						symbol_width = 30,
						symbol_type_width = 12,
						entry_maker = function(entry)
							-- Get the default entry maker
							local make_entry = require("telescope.make_entry")
							local default_entry = make_entry.gen_from_lsp_symbols({})(entry)

							-- Filter out admin.py files
							if default_entry and default_entry.filename then
								if string.match(default_entry.filename, "admin%.py$") then
									return nil
								end
							end

							return default_entry
						end,
					},
					lsp_document_symbols = {
						sorter = require("telescope").extensions.fzf.native_fzf_sorter(),
						show_line = false,
					},
					-- Enable path-based search for find_files
					find_files = {
						path_display = { "truncate" },  -- Show truncated paths from root
						find_command = {
							"rg",
							"--files",
							"--hidden",
							"--glob", "!.git/*",
							"--glob", "!node_modules/*",
							"--glob", "!.venv/*",
							"--glob", "!__pycache__/*",
						},
					},
					live_grep = {
						path_display = { "truncate" },  -- Show paths for grep results
					},
				},
			})
			require("telescope").load_extension("fzf")
		end,
	},

	"nvim-lua/plenary.nvim",

	{
		"nvim-telescope/telescope.nvim",
	},

	-- TypeScript
	{
		"pmizio/typescript-tools.nvim",
		dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
		ft = { "typescript", "typescriptreact", "javascript", "javascriptreact" },
	},

	-- Package info for package.json
	{
		"vuki656/package-info.nvim",
		dependencies = { "MunifTanjim/nui.nvim" },
		ft = "json",
		config = function()
			require("package-info").setup()
		end,
	},

	-- LSP
	"neovim/nvim-lspconfig",

	-- Schema support for JSON/YAML
	"b0o/SchemaStore.nvim",

	-- YAML companion for schema switching
	{
		"someone-stole-my-name/yaml-companion.nvim",
		dependencies = {
			"neovim/nvim-lspconfig",
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim",
		},
		ft = "yaml",
	},

	-- Diagnostics UI
	{
		"folke/trouble.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		cmd = "Trouble",
		keys = {
			{ "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>", desc = "Toggle Trouble" },
			{ "<leader>xw", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer diagnostics" },
		},
		config = function()
			require("trouble").setup()
		end,
	},

	-- Git
	{
		"lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup({
				current_line_blame = true,
				current_line_blame_opts = {
					delay = 300,
				},
			})
		end,
	},
	"tpope/vim-fugitive",

	-- Snippets
	"honza/vim-snippets",

	-- Python
	"brentyi/isort.vim",

	-- Refactoring tools
	{
		"ThePrimeagen/refactoring.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
		},
		config = function()
			require("refactoring").setup()
		end,
	},

	-- Go
	"fatih/vim-go",

	-- Editor config
	"editorconfig/editorconfig-vim",

	-- Operators
	"kana/vim-operator-user",

	-- C/C++
	"rhysd/vim-clang-format",
	"ericcurtin/CurtineIncSw.vim",

	-- Comments
	"tomtom/tcomment_vim",

	-- Languages
	"tikhomirov/vim-glsl",
	"leafgarland/typescript-vim",
	"liuchengxu/graphviz.vim",
	"prabirshrestha/async.vim",
	"martinda/Jenkinsfile-vim-syntax",
	"modille/groovy.vim",
	"rust-analyzer/rust-analyzer",

	-- Utilities
	"sk1418/HowMuch",
	"vim-scripts/loremipsum",

	{
		"nvim-pack/nvim-spectre",
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
	},

	-- Multiple cursors
	"mg979/vim-visual-multi",

	-- Auto-close brackets and quotes
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		config = function()
			require("nvim-autopairs").setup({})
		end,
	},

	-- Auto-close HTML/JSX tags
	{
		"windwp/nvim-ts-autotag",
		dependencies = "nvim-treesitter/nvim-treesitter",
		config = function()
			require("nvim-ts-autotag").setup()
		end,
	},

	-- Keybinding popup
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		config = function()
			require("which-key").setup()
		end,
	},

	-- Mini modules collection
	{
		"echasnovski/mini.nvim",
		config = function()
			require("mini.ai").setup() -- Better text objects
			require("mini.align").setup() -- Alignment
		end,
	},

	-- Better navigation
	{
		"folke/flash.nvim",
		event = "VeryLazy",
		keys = {
			{ "s", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash" },
		},
		config = function()
			require("flash").setup()
		end,
	},

	-- completion
	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter",
		config = function()
			local cmp = require("cmp")
			local luasnip = require("luasnip")

			cmp.setup({
				snippet = {
					expand = function(args)
						luasnip.lsp_expand(args.body)
					end,
				},
				window = {
					completion = cmp.config.window.bordered(),
					documentation = cmp.config.window.bordered(),
				},
				completion = {
					autocomplete = {
						cmp.TriggerEvent.TextChanged,
						cmp.TriggerEvent.InsertEnter,
					},
					completeopt = "menu,menuone,noinsert",
					keyword_length = 1,  -- Trigger after 1 character
				},
				mapping = cmp.mapping.preset.insert({
					["<C-b>"] = cmp.mapping.scroll_docs(-4),
					["<C-f>"] = cmp.mapping.scroll_docs(4),
					["<C-n>"] = cmp.mapping.complete(),  -- Changed from Ctrl+Space to Ctrl+n
					["<C-e>"] = cmp.mapping.abort(),
					["<CR>"] = cmp.mapping.confirm({ select = true }),
					["<Tab>"] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_next_item()
						elseif luasnip.expand_or_jumpable() then
							luasnip.expand_or_jump()
						else
							fallback()
						end
					end, { "i", "s" }),
					["<S-Tab>"] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_prev_item()
						elseif luasnip.jumpable(-1) then
							luasnip.jump(-1)
						else
							fallback()
						end
					end, { "i", "s" }),
				}),
				sources = cmp.config.sources({
					{ name = "nvim_lsp", priority = 1000 },
					{ name = "luasnip", priority = 750 },
					{ name = "path", priority = 500 },
					{ name = "buffer", priority = 250, keyword_length = 3 },
				}),
			})

			-- Filetype-specific settings for Python
			cmp.setup.filetype("python", {
				sources = cmp.config.sources({
					{ name = "nvim_lsp", priority = 1000 },
					{ name = "luasnip", priority = 750 },
					{ name = "path", priority = 500 },
					{ name = "buffer", priority = 250, keyword_length = 2 },
				}),
			})

			-- Filetype-specific settings for TypeScript/JavaScript/React
			cmp.setup.filetype({ "typescript", "typescriptreact", "javascript", "javascriptreact" }, {
				sources = cmp.config.sources({
					{ name = "nvim_lsp", priority = 1000 },
					{ name = "luasnip", priority = 750 },
					{ name = "path", priority = 500 },
					{ name = "buffer", priority = 250, keyword_length = 2 },
				}),
			})
		end,
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"L3MON4D3/LuaSnip",
			"saadparwaiz1/cmp_luasnip",
		},
	},

	-- Status line
	"itchyny/lightline.vim",

	-- Color schemes
	-- 'xolox/vim-misc',
	-- 'xolox/vim-colorscheme-switcher',
	"fenetikm/falcon",
	"nanotech/jellybeans.vim",
	"tomasr/molokai",
	"ayu-theme/ayu-vim",
	"therubymug/vim-pyte",
	"vim-scripts/eclipse.vim",
	"vim-scripts/summerfruit256.vim",
	"vim-scripts/AutumnLeaf",
	"vim-scripts/ironman.vim",
	"NLKNguyen/papercolor-theme",

	-- Case conversion
	"chiedo/vim-case-convert",

	{
		"L3MON4D3/LuaSnip",
		version = "v2.*",
		build = "make install_jsregexp",
	},

	-- File explorer
	{
		"stevearc/oil.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("oil").setup({
				default_file_explorer = true,
				columns = {
					"icon",
					"permissions",
					"size",
					"mtime",
				},
				view_options = {
					show_hidden = true,
				},
				float = {
					padding = 2,
					max_width = 90,
					max_height = 0,
				},
			})
		end,
	},

	-- COC
	-- {
	-- 	"neoclide/coc.nvim",
	-- 	branch = "release",
	-- 	config = function()
	-- 		vim.g.coc_enable_locationlist = 0
	-- 		vim.g.coc_global_extensions = {
	-- 			"coc-css",
	-- 			"coc-json",
	-- 			"coc-pyright",
	-- 			"coc-tsserver",
	-- 			"coc-eslint",
	-- 			"coc-prettier",
	-- 		}
	-- 	end,
	-- },
}

require("lazy").setup(plugins)
