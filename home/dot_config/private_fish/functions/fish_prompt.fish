function fish_prompt --description 'Informative prompt'
    printf '[%s@%s %s] ' $USER $hostname (prompt_pwd)
end
