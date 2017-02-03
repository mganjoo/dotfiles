#!/usr/bin/env bash

# Update submodules
git pull --recurse-submodules
git submodule update --remote --recursive

vim +PlugUpgrade +qa
vim +PlugUpdate +qa

# Remove plug.vim.old if it exists
rm -f ~/.dotfiles/vim/autoload/plug.vim.old

# Update fzf
$HOME/.external/fzf/install --completion --key-bindings --no-update-rc
