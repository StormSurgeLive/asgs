# START ASGS "list" completion
# This file is in the public domain

_list_completion()
{
    local cur
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"

    COMPREPLY=( $(compgen -W "adcirc configs meshes platforms profiles" -- "$cur") )

    return 0
}

complete -o default -F _list_completion list

# END list completion
