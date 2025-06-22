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

-- Search Settings
vim.opt.ignorecase = true     -- Case insensitive searches
vim.opt.smartcase = true      -- Ignore sensitivity setting with uppercase patterns

-- Backup
vim.opt.backup = true         -- Create backups
vim.opt.writebackup = true    -- Create backup before writing

local BACKUP_DIR = vim.fn.expand("~/.vim-backup//")
local SWAP_DIR = vim.fn.expand("~/.vim-swap//")

vim.opt.backupdir = BACKUP_DIR
vim.opt.directory = SWAP_DIR

vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    if vim.fn.isdirectory(BACKUP_DIR) == 0 then
      vim.fn.mkdir(SWAP_DIR, 'p')
    end
    
    if vim.fn.isdirectory(SWAP_DIR) == 0 then
      vim.fn.mkdir(SWAP_DIR, 'p')
    end
  end,
})

-- Folds
vim.opt.foldmethod = 'indent' -- Fold based on indent
vim.opt.foldnestmax = 4       -- Set conservative fold limit
vim.opt.foldenable = false    -- Don't show fold by default
vim.opt.foldcolumn = '1'      -- Show a single fold column

-- Appearance
vim.opt.wrap = false          -- Don't wrap lines
vim.opt.linebreak = true      -- Break lines at useful points
vim.opt.list = true           -- Show auxiliary characters
vim.opt.colorcolumn = '80'    -- Show column at 80 characters
vim.opt.laststatus = 2        -- Always show the status line
vim.opt.listchars = { tab = '▸ ', trail = '·' }

-- Color scheme
vim.cmd[[colorscheme dracula]]

-- == Enhancements == {{{1

-- Use rg instead of grep, if available
if vim.fn.executable("rg") == 1 then
  vim.opt.grepprg = "rg --vimgrep"
end

-- == python (neovim) == {{{2
local function get_brew_prefix()
  local handle = io.popen("brew --prefix")
  if handle then
    local result = handle:read("*a")
    handle:close()
    return result:gsub("%s+", "")
  end
  return "/usr/local" -- fallback
end

vim.g.python3_host_prog = get_brew_prefix() .. "/bin/python3"

-- == Keymaps == {{{1

vim.keymap.set('n', '[W', ':tabfirst<CR>', { silent = true })
vim.keymap.set('n', ']W', ':tabnext<CR>', { silent = true })
vim.keymap.set('n', '[w', ':tabprevious<CR>', { silent = true })
vim.keymap.set('n', ']w', ':tabnext<CR>', { silent = true })
vim.keymap.set('n', '<Leader>wx', ':tabclose<CR>', { silent = true })
-- Mute highlighting temporarily
vim.keymap.set('n', '<C-l>', ':<C-u>nohlsearch<CR><C-l>', { silent = true })
-- Show number of matches made by a recent search
vim.keymap.set('n', '<Leader>s', ':%s///gn<CR>', { silent = true })
vim.keymap.set('n', '<Leader>vv', function()
  vim.cmd('source ' .. vim.fn.stdpath('config') .. '/init.lua')
  print('Configuration reloaded!')
end, { silent = true })

-- == Modeline == {{{1
-- vim: foldmethod=marker:fen
