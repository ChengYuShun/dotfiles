# system
alias hibernate='sudo sh -c "sync && echo 3 > /proc/sys/vm/drop_caches &&\
  systemctl hibernate"'

# coreutils
alias ls='ls --color=auto'

# chezmoi
alias czm='chezmoi -D ~ -S ~/.local/share/chezmoi/home'
alias czmcd='env -C ~/.local/share/chezmoi/home $SHELL -i'
alias sczm='sudo -E chezmoi -D / -S ~/.local/share/chezmoi/root'
alias sczmcd='env -C ~/.local/share/chezmoi/root $SHELL -i'

# pacman
alias p='pacman --config "$XDG_CONFIG_HOME/pacman.conf"'
alias sp='sudo pacman --config "$XDG_CONFIG_HOME/pacman.conf"'
alias spyu='sudo pacman --config "$XDG_CONFIG_HOME/pacman.conf" -Syu'

# others
alias e='emacsclient -nw'
alias v='nvim'
alias c='clear'
alias r='ranger'
alias net='netctl-auto switch-to'
alias netl='netctl-auto list'
alias sqlite3='sqlite3 -init "$XDG_CONFIG_HOME/sqlite/init"'
alias sqlite='sqlite3'
alias py='python'
alias et='exec tmux'
