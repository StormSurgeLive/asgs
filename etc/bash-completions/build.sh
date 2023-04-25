# START ASGS "build" completion
# This file is in the public domain

_build_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    
    case $prev in
      adcirc|jq|pdl|perl-dev|replaycli)
        COMPREPLY=""
        ;;
      *)
        COMPREPLY=( $(compgen -W "adcirc jq pdl perl-dev replaycli " -- $cur) )
	;;
    esac
    return 0
} &&
  complete -F _build_completion build

# END build completion
