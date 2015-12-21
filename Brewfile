## == Packages ==

cask_args appdir: '/Applications'
tap 'caskroom/cask'
tap 'caskroom/versions'
tap 'homebrew/versions'

# Dotfiles sync tool
brew 'thoughtbot/formulae/rcm'

# Version control
brew 'git'
brew 'git-extras'

# Programming tools
brew 'vim', args: ['with-lua', 'override-system-vi']
brew 'ag'
brew 'ctags'
brew 'cscope'
brew 'homebrew/dupes/grep', args: ['default-names']
brew 'lesspipe', args: ['syntax-highlighting']
brew 'markdown'
brew 'ssh-copy-id'

# Terminal
brew 'readline'
brew 'bash'
brew 'zsh'
brew 'zsh-completions'
brew 'terminal-notifier'
brew 'fswatch'
brew 'tree'

# Tmux
brew 'tmux'
brew 'reattach-to-user-namespace'

# Languages
brew 'python'
brew 'python3'

# Note: requires Xcode
brew 'macvim', args: ['with-lua']

# Diff tools
brew 'colordiff'
brew 'wdiff'

## == Casks ==

# Essential
cask 'dropbox'
cask 'google-chrome', args: { appdir: '/Applications' }

# Mac Utilities
cask '1password'
cask 'alfred', args: { appdir: '/Applications' }
cask 'appcleaner'
cask 'bartender'
cask 'flux'
cask 'textexpander'
cask 'the-unarchiver'
cask 'hammerspoon'
cask 'shortcat'

# Developer
cask 'r'
cask 'rstudio'
cask 'iterm2'
cask 'intellij-idea-ce'

# Organizer
cask 'omnifocus'
cask 'fantastical'
cask 'evernote'
cask 'skitch'
cask 'ynab'
cask 'vitamin-r'

# Media
cask 'spotify'
cask 'vlc'

# Social
cask 'skype'

# Haskell
cask 'ghc', args: { appdir: '/Applications' }
cask 'stack'
cask 'pandoc'

# Java
cask 'java' unless system '/usr/libexec/java_home --failfast'

# TeX
cask 'mactex'
