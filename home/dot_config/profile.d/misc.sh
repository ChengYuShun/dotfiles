USE_FISH=1
EDITOR=nvim

# host specific settings
if [ "$(hostname)" = pretty-arch ]; then
  EDITOR="emacsclient -nw"
fi

# OS specific settings
if [ "$UNAME_S" = Linux ]; then
  DMI_PRODUCT_NAME="$(cat /sys/devices/virtual/dmi/id/product_name)"
elif [ "$UNAME_S" = Msys ]; then
  EDITOR=vim
fi

# export variables
envexp USE_FISH
envexp EDITOR
envexp DMI_PRODUCT_NAME
