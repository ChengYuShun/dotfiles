if [ "$XDG_SESSION_TYPE" = tty ] && [ -z "$SSH_TTY" ]; then
  if which setfont 1>/dev/null 2>/dev/null; then
    setfont /usr/share/kbd/consolefonts/ter-118n.psf.gz
  fi
fi
