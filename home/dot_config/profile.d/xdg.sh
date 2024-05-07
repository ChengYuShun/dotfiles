envexp HOME
envexp XDG_CONFIG_HOME "$HOME/.config"
envexp XDG_DATA_HOME "$HOME/.local/share"
envexp XDG_CACHE_HOME "$HOME/.cache"

# misc.
envexp ZDOTDIR "$XDG_CONFIG_HOME/zsh"
envexp AGDA_DIR "$XDG_CONFIG_HOME/agda"
envexp STACK_XDG 1

# OS specific settings
if [ "$UNAME_S" = Linux ]; then
  envexp SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent"
fi
