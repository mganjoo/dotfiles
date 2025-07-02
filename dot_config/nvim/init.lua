-- == Prologue == {{{1

-- Setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
vim.g.mapleader = ' '
vim.g.maplocalleader = ','

-- == Plugin Management == {{{1

require("config.lazy")

-- == Basic Settings == {{{1

-- Syntax & Indentation
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2
vim.opt.expandtab = true   -- Expand tab to spaces
vim.opt.shiftround = true  -- Round indent to multiple of shiftwidth
vim.opt.copyindent = true  -- Copy indent structure from previous line

-- General Settings
vim.opt.autowrite = true      -- Save before :next, :make etc
vim.opt.cursorline = true     -- Show line cursor is on
vim.opt.number = true         -- Always show line numbers
vim.opt.relativenumber = true -- Use relative numbers by default
vim.opt.shortmess:append('I') -- Disable startup message
vim.opt.mouse = 'a'           -- Enable mouse mode
vim.opt.clipboard = 'unnamed' -- Sync clipboard
vim.opt.showmode = false      -- Don't set current status on the last line

-- Search Settings
vim.opt.ignorecase = true     -- Case insensitive searches
vim.opt.smartcase = true      -- Ignore sensitivity setting with uppercase patterns

-- Backup
vim.opt.backup = true         -- Create backups
vim.opt.writebackup = true    -- Create backup before writing

-- Disable netrw because we use vim-tree
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- LSP
vim.lsp.inlay_hint.enable(true) -- Enable inlay hints for LSP

-- Persist undo (persists undo history between sessions)
vim.opt.undodir = vim.fn.stdpath('cache') .. '/undo'
vim.opt.undofile = true

-- Auto-create backup and swap directories
local BACKUP_DIR = vim.fn.expand("~/.vim-backup//")
local SWAP_DIR = vim.fn.expand("~/.vim-swap//")
vim.opt.backupdir = BACKUP_DIR
vim.opt.directory = SWAP_DIR
vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    if vim.fn.isdirectory(BACKUP_DIR) == 0 then
      vim.fn.mkdir(BACKUP_DIR, 'p')
    end
    if vim.fn.isdirectory(SWAP_DIR) == 0 then
      vim.fn.mkdir(SWAP_DIR, 'p')
    end
  end,
})

-- Appearance
vim.opt.wrap = false          -- Don't wrap lines
vim.opt.linebreak = true      -- Break lines at useful points
vim.opt.list = true           -- Show auxiliary characters
vim.opt.colorcolumn = '80'    -- Show column at 80 characters
vim.opt.laststatus = 2        -- Always show the status line
vim.opt.listchars = { tab = '▸ ', trail = '·' }
vim.opt.termguicolors = true  -- Enable 24-bit color

-- == Enhancements == {{{1

-- Use rg instead of grep, if available
if vim.fn.executable("rg") == 1 then
  vim.opt.grepprg = "rg --vimgrep"
end

local function get_brew_prefix()
  local handle = io.popen("brew --prefix")
  if handle then
    local result = handle:read("*a")
    handle:close()
    return result:gsub("%s+", "")
  end
  return "/usr/local" -- fallback
end

-- == Keymaps == {{{1

-- Mute highlighting temporarily
vim.keymap.set('n', '<C-l>', ':<C-u>nohlsearch<cr><C-l>', { silent = true })
-- Show number of matches made by a recent search
vim.keymap.set('n', '<leader>s', ':%s///gn<cr>', { silent = true })
-- Reload vim config after edits
vim.keymap.set('n', '<leader>vv', function()
  vim.cmd('source ' .. vim.fn.stdpath('config') .. '/init.lua')
  print('Configuration reloaded!')
end, { silent = true })
-- Shortcuts for nvimdiff mode
if vim.opt.diff:get() then
  vim.keymap.set('n', '<leader>1', ':diffget LOCAL<cr>')
  vim.keymap.set('n', '<leader>2', ':diffget BASE<cr>')
  vim.keymap.set('n', '<leader>3', ':diffget REMOTE<cr>')
end

-- == LSP === {{{1

-- Set up folding, default to treesitter unless LSP supports it
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter#foldexpr()"
vim.opt.foldlevel = 99
vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client:supports_method('textDocument/foldingRange') then
      local win = vim.api.nvim_get_current_win()
      vim.wo[win][0].foldexpr = 'v:lua.vim.lsp.foldexpr()'
    end
  end,
})

-- == Modeline == {{{1
-- vim: foldmethod=marker:fen
