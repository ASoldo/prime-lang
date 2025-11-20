return {
	-- LSP
	{
		"neovim/nvim-lspconfig",
		ft = { "prime" },
		config = function()
			vim.filetype.add({
				extension = { prime = "prime" },
			})

			local lspconfig = require("lspconfig")
			local configs = require("lspconfig.configs")
			local capabilities = require("blink.cmp").get_lsp_capabilities()

			if not configs.primelang then
				configs.primelang = {
					default_config = {
						cmd = { "prime-lang", "lsp" },
						filetypes = { "prime" },
						root_dir = function(fname)
							local git = vim.fs.find(".git", { path = fname, upward = true })[1]
							if git then
								return vim.fs.dirname(git)
							end
							return vim.fs.dirname(fname)
						end,
					},
				}
			end

			lspconfig.primelang.setup({
				capabilities = capabilities,
				on_attach = function(client, bufnr)
					local map = function(lhs, rhs, desc)
						vim.keymap.set("n", lhs, rhs, { buffer = bufnr, desc = desc })
					end
					map("<leader>lg", vim.lsp.buf.definition, "Go to definition (Prime)")
					map("<leader>ll", vim.lsp.buf.declaration, "Go to declaration (Prime)")
					map("<leader>lf", vim.lsp.buf.references, "Go to references (Prime)")
					map("<leader>lr", vim.lsp.buf.rename, "Rename symbol (Prime)")
					map("<leader>la", vim.lsp.buf.code_action, " Code Action (Prime)")
					map("<leader>lw", vim.lsp.buf.workspace_symbol, " Workspace Symbols (Prime)")
					if client.server_capabilities.documentFormattingProvider then
						vim.api.nvim_create_autocmd("BufWritePre", {
							buffer = bufnr,
							callback = function()
								vim.lsp.buf.format({ bufnr = bufnr })
							end,
						})
					end
				end,
			})
		end,
	},
	-- Prime Icon
	{
		"nvim-mini/mini.icons",
		-- AstroNvim already loads this; we just extend its config
		opts = function(_, opts)
			opts = opts or {}

			-- make sure categories exist
			opts.extension = opts.extension or {}
			opts.filetype = opts.filetype or {}
			opts.directory = opts.directory or {}

			-- 1) .prime files
			opts.extension["prime"] = {
				glyph = "",
				hl = "MiniIconsCyan",
			}
			opts.filetype["prime"] = opts.extension["prime"]

			-- 2) Folders for prime-lang projects
			-- folder literally named ".build.prime"
			opts.directory[".build.prime"] = {
				glyph = "",
				hl = "MiniIconsCyan",
			}

			-- folder named "src" (yeah, global; good enough for your own projects)
			opts.directory["src"] = {
				glyph = "",
				hl = "MiniIconsCyan",
			}

			return opts
		end,
	},
	-- TreeSitter
	{
		"stevearc/aerial.nvim",
		opts = {
			backends = { "treesitter", "lsp" },
			filter_kind = false,
		},
	},
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		opts = function(_, opts)
			opts = opts or {}
			opts.highlight = opts.highlight or {}
			opts.highlight.enable = true
			if type(opts.highlight.disable) == "table" then
				for i = #opts.highlight.disable, 1, -1 do
					if opts.highlight.disable[i] == "prime" then
						table.remove(opts.highlight.disable, i)
					end
				end
			end

			vim.filetype.add({ extension = { prime = "prime" } })

			local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
			---@diagnostic disable-next-line: inject-field
			parser_config.prime = {
				install_info = {
					url = "https://github.com/asoldo/prime_lang_treesitter.git",
					files = { "src/parser.c" },
					branch = "main",
				},
				filetype = "prime",
			}
			vim.treesitter.language.register("prime", "prime")

			local query_root = vim.fn.stdpath("data") .. "/site/queries/prime"
			vim.fn.mkdir(query_root, "p")

			-- highlights.scm
			do
				local highlights = [[
    ["fn" "let" "import" "struct" "enum" "const" "match" "if" "else" "for" "in" "while" "return" "defer" "module" "pub" "interface" "impl" "try" "move"] @keyword

    (module_declaration name: (module_path) @namespace)
    (module_path (identifier) @namespace)
    (import_declaration path: (module_path) @namespace)
    (import_declaration path: (string_literal) @namespace)

    (function_definition name: (identifier) @function)
    (parameter (identifier) @variable.parameter)
    (parameter type: (type_expression) @type)
    (call_expression
      function: (identifier) @function.call)
    (call_expression
      function: (field_expression field: (identifier) @function.call))
    (call_expression
      arguments: (argument_list (identifier) @variable))

    (struct_definition name: (type_identifier) @type)
    (enum_definition name: (type_identifier) @type)
    (interface_definition name: (type_identifier) @type)
    (impl_definition target: (type_expression) @type)
    (const_definition name: (identifier) @constant)

    (let_statement name: (identifier) @variable)
    (let_statement pattern: (identifier) @variable)
    (let_statement annotation: (type_expression) @type)
    (pattern (identifier) @variable)
    (pattern (tuple_pattern (identifier) @variable))

    (type_expression (type_identifier) @type)
    (type_expression (type_path (type_identifier)) @type)
    (type_expression (module_path) @type)
    (type_expression (identifier) @type)
    (reference_type (type_identifier) @type)
    (reference_type (identifier) @type)
    (pointer_type (type_identifier) @type)
    (pointer_type (identifier) @type)
    (slice_type (type_identifier) @type)
    (slice_type (identifier) @type)
    (array_type (type_identifier) @type)
    (array_type (identifier) @type)
    (tuple_type (type_identifier) @type)
    (tuple_type (identifier) @type)
    ((identifier) @type.builtin
      (#match? @type.builtin "^(int|uint)(8|16|32|64)$|^u?size$|^float(32|64)$|^bool$|^string$|^rune$"))

    (struct_literal_field name: (identifier) @field)
    (map_entry key: (identifier) @field)
    (map_entry key: (string_literal) @string)
    (field_expression field: (identifier) @field)

    (assign_statement target: (identifier) @variable)
    (assign_statement target: (unary_expression (identifier) @variable))
    (assign_statement value: (identifier) @variable)

    ;; Constructors / enum variants (Ok, Err, etc.)
    (call_expression
      function: (identifier) @constructor
      (#match? @constructor "^[A-Z]"))
    (enum_pattern
      variant: (_) @constructor
      (#match? @constructor "^[A-Z]"))

    (integer_literal) @number
    (float_literal)   @float
    (string_literal)  @string
    (format_string_literal) @string.special
    (rune_literal)    @character
    (boolean_literal) @boolean
    ;; catch-all variables
    ((identifier) @variable
      (#match? @variable "^[a-z_]")
      (#not-match? @variable "^(int|uint)(8|16|32|64)$|^u?size$|^float(32|64)$|^bool$|^string$|^rune$"))
    ]]
				local f = assert(io.open(query_root .. "/highlights.scm", "w"))
				f:write(highlights)
				f:close()
			end

			-- aerial.scm
			do
				local aerial = [[
    (module_declaration
      name: (module_path) @name
      (#set! "kind" "Namespace")) @symbol

    (struct_definition
      name: (type_identifier) @name
      (#set! "kind" "Struct")) @symbol

    (enum_definition
      name: (type_identifier) @name
      (#set! "kind" "Enum")) @symbol

    (interface_definition
      name: (type_identifier) @name
      (#set! "kind" "Interface")) @symbol

    (impl_definition
      target: (type_expression) @name
      (#set! "kind" "Class")) @symbol

    (function_definition
      name: (identifier) @name
      (#set! "kind" "Function")) @symbol

    (const_definition
      name: (identifier) @name
      (#set! "kind" "Constant")) @symbol

    (let_statement
      name: (identifier) @name
      (#set! "kind" "Variable")) @symbol

    (let_statement
      pattern: (identifier) @name
      (#set! "kind" "Variable")) @symbol

    (assign_statement
      target: (identifier) @name
      (#set! "kind" "Variable")) @symbol
    ]]
				local f = assert(io.open(query_root .. "/aerial.scm", "w"))
				f:write(aerial)
				f:close()
			end

			return opts
		end,
	},
}
