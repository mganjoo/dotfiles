return {
  {
    -- auto-closing quotes and brackets
    'windwp/nvim-autopairs',
    event = 'InsertEnter',
    config = function()
      require('nvim-autopairs').setup({ check_ts = true })
    end,
  },
  {
    -- shows the corresponding closing paren for an opening paren
    'andymass/vim-matchup',
    init = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
    end,
  },
  { 'tpope/vim-surround' },
  {
    'catppuccin/nvim',
    priority = 1000,
    config = function()
      require('catppuccin').setup({})
      vim.cmd[[colorscheme catppuccin]]
    end,
  },
  {
    'nvim-tree/nvim-tree.lua',
    config = true,
    keys = {
      { '<leader>t', '<cmd>NvimTreeFindFileToggle<cr>', desc = 'Open nvim-tree' },
    },
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      require('lualine').setup({
        options = { theme = 'catppuccin' }
      })
    end,
  },
  {
    'f-person/auto-dark-mode.nvim',
    config = true,
  },
  { 'lewis6991/gitsigns.nvim' },
  {
    'nvim-telescope/telescope.nvim',
    dependencies = { 
      'nvim-lua/plenary.nvim',
      { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' }
    },
    config = function()
      require('telescope').setup({
        pickers = {
          find_files = {
            hidden = true,
          },
        },
      })
      require('telescope').load_extension('fzf')
    end,
    keys = {
      { '<leader> ', '<cmd>Telescope find_files<cr>', desc = 'Find files' },
      { '<leader>fg', '<cmd>Telescope live_grep<cr>', desc = 'Live grep' },
      { '<leader>fb', '<cmd>Telescope buffers<cr>', desc = 'Buffers' },
      { '<leader>fh', '<cmd>Telescope help_tags<cr>', desc = 'Help tags' },
      { '<leader>fc', '<cmd>Telescope commands<cr>', desc = 'Commands' },
    },
  },
  {
    'nvim-treesitter/nvim-treesitter',
    branch = 'master',
    lazy = false,
    build = ":TSUpdate",
    config = function()
      local configs = require('nvim-treesitter.configs')
      configs.setup({
        ensure_installed = {
          'lua',
          'python',
          'typescript',
          'javascript',
          'html',
        },
        auto_install = true,
        sync_install = false,
        highlight = { enable = true },
        indent = { enable = true },
        matchup = { enable = true },
      })
      vim.opt.foldmethod = "expr"
      vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
      vim.opt.foldlevel = 99
    end,
  },
  { 'neovim/nvim-lspconfig' },
  {
    -- Allows editing directories like a buffer
    'stevearc/oil.nvim',
    config = true,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    lazy = false,
  },
  { 'gbprod/substitute.nvim' },
  { 'tpope/vim-unimpaired' },
  { 'troydm/zoomwintab.vim' },
  { 'tpope/vim-repeat' },
  { 'tpope/vim-eunuch' },
  { 'google/vim-jsonnet' },
  {
    'folke/which-key.nvim',
    event = 'VeryLazy',
    keys = {
      {
        '<leader>?',
        function()
          require('which-key').show({ global = false })
        end,
        desc = 'Buffer Local Keymaps (which-key)',
      },
    },
  },
  {
    'akinsho/bufferline.nvim',
    version = "*",
    config = true,
    dependencies = 'nvim-tree/nvim-web-devicons',
  },
}
