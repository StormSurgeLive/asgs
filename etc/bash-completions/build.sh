# START ASGS "build" completion
# This file is in the public domain

_build_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    
    COMPREPLY=( $(compgen -W "adcirc jq pdl perl-dev replaycli " -- $cur) )
    return 0
} &&
  complete -F _build_completion build

# END build completion
