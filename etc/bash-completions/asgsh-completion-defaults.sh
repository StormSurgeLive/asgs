# ASGS bash completion
# This file is in the public domain

# -------------------------------------------------------------------
# shared helpers
# -------------------------------------------------------------------

_asgs_comp_cur_prev()
{
    ASGS_CUR="${COMP_WORDS[COMP_CWORD]}"
    ASGS_PREV="${COMP_WORDS[COMP_CWORD-1]}"
}

_asgs_complete_words()
{
    local words="$1"
    COMPREPLY=( $(compgen -W "$words" -- "$ASGS_CUR") )
}

_asgs_complete_files()
{
    COMPREPLY=( $(compgen -f -- "$ASGS_CUR") )
    compopt -o filenames 2>/dev/null
}

_asgs_complete_dirs()
{
    COMPREPLY=( $(compgen -d -- "$ASGS_CUR") )
    compopt -o filenames 2>/dev/null
}

# -------------------------------------------------------------------
# build
# -------------------------------------------------------------------

_asgs_build_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    case "$ASGS_PREV" in
        adcirc|jq|pdl|perl-dev|replaycli)
            return 0
            ;;
        *)
            _asgs_complete_words "adcirc jq pdl perl-dev replaycli"
            ;;
    esac

    return 0
}

# -------------------------------------------------------------------
# clone
# -------------------------------------------------------------------

_asgs_clone_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    case "$ASGS_PREV" in
        profile)
            return 0
            ;;
        *)
            _asgs_complete_words "profile"
            ;;
    esac

    return 0
}

# -------------------------------------------------------------------
# define
# -------------------------------------------------------------------

_asgs_define_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    case "$ASGS_PREV" in
        config)
            _asgs_complete_files
            ;;
        editor)
            _asgs_complete_words "vim vi nano emacs"
            compopt -o nospace 2>/dev/null
            ;;
        adcircdir|adcircbranch|adcircremote|scriptdir|scratchdir|workdir)
            return 0
            ;;
        *)
            _asgs_complete_words "adcircdir adcircbranch adcircremote config editor scriptdir scratchdir workdir"
            ;;
    esac

    return 0
}

# -------------------------------------------------------------------
# delete
# -------------------------------------------------------------------

_asgs_delete_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    case "$ASGS_PREV" in
        adcirc|config|profile|statefile)
            return 0
            ;;
        *)
            _asgs_complete_words "adcirc config profile statefile"
            ;;
    esac

    return 0
}

# -------------------------------------------------------------------
# edit
# -------------------------------------------------------------------

_asgs_edit_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    case "$ASGS_PREV" in
        adcirc|config|profile|statefile|syslog)
            return 0
            ;;
        *)
            _asgs_complete_words "adcirc config profile statefile syslog"
            ;;
    esac

    return 0
}

# -------------------------------------------------------------------
# goto
# -------------------------------------------------------------------

_asgs_goto_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    case "$ASGS_PREV" in
        adcircworkdir|adcircdir|installdir|lastsubdir|rundir|scratchdir|scriptdir|workdir|statefile)
            return 0
            ;;
        *)
            _asgs_complete_words "adcircworkdir adcircdir installdir lastsubdir rundir scratchdir scriptdir workdir"
            ;;
    esac

    return 0
}

# -------------------------------------------------------------------
# init
# -------------------------------------------------------------------

_asgs_init_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    case "$ASGS_PREV" in
        config|keys)
            return 0
            ;;
        *)
            _asgs_complete_words "config keys"
            ;;
    esac

    return 0
}

# -------------------------------------------------------------------
# list
# -------------------------------------------------------------------

_asgs_list_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    _asgs_complete_words "adcirc configs meshes platforms profiles"

    return 0
}

# -------------------------------------------------------------------
# load
# -------------------------------------------------------------------

_asgs_load_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    case "$ASGS_PREV" in
        adcirc|profile)
            return 0
            ;;
        *)
            _asgs_complete_words "adcirc profile"
            ;;
    esac

    return 0
}

# -------------------------------------------------------------------
# move
# -------------------------------------------------------------------

_asgs_move_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    case "$ASGS_PREV" in
        statefile)
            return 0
            ;;
        *)
            _asgs_complete_words "statefile"
            ;;
    esac

    return 0
}

# -------------------------------------------------------------------
# rebuild
# -------------------------------------------------------------------

_asgs_rebuild_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    case "$ASGS_PREV" in
        profile)
            _asgs_complete_files
            ;;
        *)
            _asgs_complete_words "profile"
            ;;
    esac

    return 0
}

# -------------------------------------------------------------------
# tailf
# -------------------------------------------------------------------

_asgs_tailf_completion()
{
    _asgs_comp_cur_prev
    COMPREPLY=()

    case "$ASGS_PREV" in
        mail-log|syslog)
            return 0
            ;;
        *)
            _asgs_complete_words "mail-log syslog"
            ;;
    esac

    return 0
}

# START ASGS "inspect" completion
# This file is in the public domain

_inspect_completion()
{
    local cur prev
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev" in
        adcirc|profile)
            return 0
            ;;
        asgs-global|asgsh-profile|config|hostfile|mail-log|meshes|platforms|ssh-config|statefile|syslog)
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "adcirc asgs-global asgsh-profile config hostfile mail-log meshes platforms profile ssh-config statefile syslog" -- "$cur") )
            ;;
    esac

    return 0
}

# END inspect completion

# START ASGS "purge" completion
# This file is in the public domain

_purge_completion()
{
    local cur prev
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev" in
        rundir|scratchdir)
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "rundir scratchdir" -- "$cur") )
            ;;
    esac

    return 0
}

# END purge completion

# START ASGS "switch" completion
# This file is in the public domain

_switch_completion()
{
    local cur prev
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev" in
        adcirc|profile)
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "adcirc profile" -- "$cur") )
            ;;
    esac

    return 0
}

# END switch completion

# -------------------------------------------------------------------
# command registration
# -------------------------------------------------------------------

complete -o default -F _asgs_build_completion   build
complete -o default -F _asgs_clone_completion   clone
complete -o default -F _asgs_define_completion  define
complete -o default -F _asgs_delete_completion  delete
complete -o default -F _asgs_edit_completion    edit
complete -o default -F _asgs_goto_completion    goto
complete -o default -F _asgs_init_completion    init
complete -o default -F _asgs_list_completion    list
complete -o default -F _asgs_load_completion    load
complete -o default -F _asgs_move_completion    move
complete -o default -F _asgs_rebuild_completion rebuild
complete -o default -F _asgs_tailf_completion   tailf
complete -o default -F _inspect_completion      inspect
complete -o default -F _purge_completion        purge
complete -o default -F _switch_completion       switch
