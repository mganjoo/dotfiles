let s:lightlineThemeFiles = globpath(&rtp, "autoload/lightline/colorscheme/solarized.vim", 1, 1)

function! auto_switch_colors#isDarkMode()
  let l:result = get(systemlist("defaults read -g AppleInterfaceStyle 2>&1 | grep Dark"), 0, "light")
  if l:result == "dark"
    return 1
  else
    return 0
  endif
endfunction

function auto_switch_colors#updateUI()
  silent! colorscheme solarized
  for l:f in s:lightlineThemeFiles
    execute "source" l:f
  endfor
  call lightline#init()
  call lightline#colorscheme()
  call lightline#update()
endfunction

function! auto_switch_colors#setDarkMode()
  set background=dark
  call auto_switch_colors#updateUI()
endfunction

function! auto_switch_colors#setLightMode()
  set background=light
  call auto_switch_colors#updateUI()
endfunction

function auto_switch_colors#switch()
  let l:isDark = auto_switch_colors#isDarkMode()
  let l:currentBackground = &background
  if l:isDark && l:currentBackground != "dark"
    call auto_switch_colors#setDarkMode()
  elseif !l:isDark && l:currentBackground != "light"
    call auto_switch_colors#setLightMode()
  endif
endfunction

function! auto_switch_colors#colorschemeChanged()
  let s:auto_switch_colors_autocmd_allowed = 0
endfunction

function! auto_switch_colors#autoChange()
  let s:auto_switch_colors_autocmd_allowed = 1
  call auto_switch_colors#switch()
endfunction

command! SetDarkMode call auto_switch_colors#setDarkMode()
command! SetLightMode call auto_switch_colors#setLightMode()

augroup auto_switch_colors
  autocmd!
  autocmd ColorScheme * nested call auto_switch_colors#colorschemeChanged()
  autocmd CursorHold,CursorHoldI,FocusGained,FocusLost * nested call auto_switch_colors#autoChange()
augroup END

if !g:auto_switch_colors_initial_load
  if auto_switch_colors#isDarkMode()
    call auto_switch_colors#setDarkMode()
  else
    call auto_switch_colors#setLightMode()
  endif
  let g:auto_switch_colors_initial_load = 1
endif
