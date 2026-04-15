# START ASGS "rebuild" completion
# This file is in the public domain

_rebuild_completion()
{
    local cur prev
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev" in
        profile)
            COMPREPLY=( $(compgen -f -- "$cur") )
            compopt -o filenames
            ;;
        *)
            COMPREPLY=( $(compgen -W "profile" -- "$cur") )
            ;;
    esac

    return 0
}

complete -o default -F _rebuild_completion rebuild

# END rebuild completion
