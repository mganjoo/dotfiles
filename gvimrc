set guioptions-=rL " Remove left and right scrollbars
set guioptions-=T  " Remove toolbar
set clipboard=unnamed " Clipboard sync

" Set fonts and platform-specific stuff
" TODO: streamline installation of these fonts
if has("gui_gtk2") && match(system("cat /etc/issue"), "Ubuntu") != -1
  set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 14
elseif has("gui_macvim")
  set guifont=Meslo\ LG\ S\ DZ\ for\ Powerline:h14
endif
