# Proxies.
envsrc xdg
envsrc path
if [ -f "$XDG_CONFIG_HOME/clash/settings-private.yaml" ]; then
  PROXY_AUTH=$(yq -r '.["authentication"][0]' \
                  "$XDG_CONFIG_HOME/clash/settings-private.yaml")
  if [ "$PROXY_AUTH" = null ]; then
    unset -v PROXY_AUTH
  else
    envexp PROXY_AUTH
    envexp ALL_PROXY "socks5h://$PROXY_AUTH@127.0.0.1:7891"
  fi
fi
if [ "$PROXY_AUTH" = "" ]; then
  envexp ALL_PROXY socks5h://127.0.0.1:7891
fi
envexp http_proxy "$ALL_PROXY"

