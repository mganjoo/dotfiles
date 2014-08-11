let s:theme = substitute(system("~/.bin/theme"), "\n", "", "")
execute "set background=".s:theme
set t_Co=256
silent! colorscheme solarized
