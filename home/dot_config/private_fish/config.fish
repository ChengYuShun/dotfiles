set -l FISH_CONFIG_HOME $XDG_CONFIG_HOME'/fish'

if status is-interactive
    # Aliases.
    source $FISH_CONFIG_HOME'/aliases.fish'
    alias spu='sudo-pacman-update'

    fish_vi_key_bindings
end

set -e FISH_CONFIG_HOME
