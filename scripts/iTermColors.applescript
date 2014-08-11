-- Set theme of all open iTerm windows and tmux sessions.
if application "iTerm" is running then
tell application "iTerm"
  set tint to do shell script "~/.bin/theme"
  try
    repeat with t in terminals
      tell t
        repeat with s in sessions
          try
            get s
          on error
            set s to launch session "Default Session"
          end try
          tell s
            if tint is "light" then
              set selected text color to {1.813483984375E+4, 2.337368359375E+4, 2.50989140625E+4}
              set cursor_text color to {6.003730859375E+4, 5.83269609375E+4, 5.2284546875E+4}
              set bold color to {1.813483984375E+4, 2.337368359375E+4, 2.50989140625E+4}
              set background color to {6.48425703125E+4, 6.277885546875E+4, 5.662616015625E+4}
              set cursor color to {2.1257337890625E+4, 2.6684328125E+4, 2.8737466796875E+4}
              set selection color to {6.003730859375E+4, 5.83269609375E+4, 5.2284546875E+4}
              set foreground color to {2.1257337890625E+4, 2.6684328125E+4, 2.8737466796875E+4}
            else
              set selected text color to {3.31601796875E+4, 3.70179921875E+4, 3.6937921875E+4}
              set cursor_text color to {0.0, 1.020768359375E+4, 1.2694220703125E+4}
              set bold color to {3.31601796875E+4, 3.70179921875E+4, 3.6937921875E+4}
              set background color to {0.0, 7722.3891601562, 9941.8388671875}
              set cursor color to {2.887342578125E+4, 3.339855859375E+4, 3.38722890625E+4}
              set selection color to {0.0, 1.020768359375E+4, 1.2694220703125E+4}
              set foreground color to {2.887342578125E+4, 3.339855859375E+4, 3.38722890625E+4}
            end if
          end tell
        end repeat
      end tell
    end repeat
  end try

  # Reload tmux configuration
  set tmuxscript to "/usr/local/bin/tmux source-file ~/.tmux-colors-" & tint & ".conf || true"
  do shell script tmuxscript
end tell
end if
