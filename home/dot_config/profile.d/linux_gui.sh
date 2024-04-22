if [ -n "$XDG_SESSION_TYPE" ] && [ "$XDG_SESSION_TYPE" != tty ]; then
  envexp TERMINAL alacritty
  envexp BROWSER "firefox -P cys"
  envexp SSH_ASKPASS /usr/bin/ksshaskpass
  # input method
  envexp INPUT_METHOD fcitx
  envexp XMODIFIERS "@im=fcitx"
  # DPI
  envsrc misc
  case "$DMI_PRODUCT_NAME" in
    "HP EliteBook 755 G5" )
      envexp DPI 144
      envexp DPI_SCALE 1.5
      ;;
  esac
  # wayland
  if [ "$XDG_SESSION_TYPE" = wayland ]; then
    envexp MOZ_ENABLE_WAYLAND 1
    envexp GDK_BACKEND wayland
    envexp QT_QPA_PLATFORM wayland
    envexp SDL_VIDEODRIVER wayland
  fi
  # desktop specific settings
  case "$XDG_CURRENT_DESKTOP" in
    GNOME )
      envexp QT_QPA_PLATFORMTHEME gtk3
      ;;
  esac
fi
