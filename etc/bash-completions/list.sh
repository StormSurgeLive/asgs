# START ASGS "list" completion
# This file is in the public domain

_list_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case $prev in
      *)
        COMPREPLY=( $(compgen -W "adcirc configs meshes platforms profiles" -- $cur) )
        ;;
    esac
    return 0
} &&
  complete -F _list_completion list

# END list completion
