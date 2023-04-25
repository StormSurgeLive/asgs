# START ASGS "rebuild" completion
# This file is in the public domain

_rebuild_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    
    case $prev in
      profile)
        COMPREPLY=( $(compgen -d -- $cur) )
        COMPREPLY=( $(compgen -f -- $cur) )
        compopt -o filenames 
        ;;
    esac
    return 0
} &&
  complete -F _rebuild_completion rebuild

# END rebuild completion
