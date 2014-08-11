" Customization
" Loads post-configuration settings

if filereadable(expand("~/.vimrc.after"))
  source ~/.vimrc.after
endif
