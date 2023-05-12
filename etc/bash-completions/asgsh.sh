# START ASGS "goto" completion
# This file is in the public domain

_goto_completion() 
{
    local cur prev files 
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    COMPREPLY=( $(compgen -W "load goto sd rd lsd tailf rebuild clone edit" -- $cur) )
    return 0
} &&
  complete -F _goto_completion goto

# END goto completion
