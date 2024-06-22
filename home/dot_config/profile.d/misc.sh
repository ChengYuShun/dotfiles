USE_FISH=1
EDITOR=nvim

# host specific settings
if [ "$(hostname)" = pretty-arch ]; then
  EDITOR="emacsclient -nw"
fi

# OS specific settings
if [ "$UNAME_S" = Linux ]; then
  DMI_PRODUCT_NAME_PATH=/sys/devices/virtual/dmi/id/product_name
  if [ -e "$DMI_PRODUCT_NAME_PATH" ]; then
    DMI_PRODUCT_NAME="$(cat "$DMI_PRODUCT_NAME_PATH")"
  fi
  unset -v DMI_PRODUCT_NAME_PATH
elif [ "$UNAME_S" = Msys ]; then
  EDITOR=vim
fi

# export variables
envexp USE_FISH
envexp EDITOR
envexp DMI_PRODUCT_NAME
