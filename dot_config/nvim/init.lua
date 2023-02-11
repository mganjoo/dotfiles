local vimrc = vim.fn.stdpath('config') .. '/vimrc.vim'
vim.cmd.source(vimrc)

local auto_dark_mode = require('auto-dark-mode')

auto_dark_mode.setup({
  update_interval = 1000,
  set_dark_mode = function()
    require('github-theme').setup({
      theme_style = "dark"
    })
  end,
  set_light_mode = function()
    require('github-theme').setup({
      theme_style = "light"
    })
  end,
})

auto_dark_mode.init()
