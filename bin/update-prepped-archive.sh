#!/usr/bin/env bash
#----------------------------------------------------------------------------
# update-prepped-archive.sh
#
# Rebuild the ASGS forecast preprocessing archive from an optimized
# partmesh.txt in an existing ASGS run directory.
#
# The script:
#   1. loads the configuration named by $ASGS_CONFIG;
#   2. determines the active NCPU, INSTANCENAME, SCRATCH, and ADCIRCDIR;
#   3. validates the supplied partmesh.txt against the full-domain fort.14;
#   4. runs adcprep --prepall in an isolated temporary directory;
#   5. validates the generated PE*/fort.14 and PE*/fort.18 files; and
#   6. atomically replaces the forecast PREPPEDARCHIVE under $SCRATCH.
#
# Existing forecast scenarios that have already extracted their PE directories
# are not changed. This archive will be used by scenarios prepped afterward.
#
# Usage:
#   update-prepped-archive.sh
#   update-prepped-archive.sh /path/to/optimized-partmesh.txt
#
# With no argument, ./partmesh.txt is used.
#----------------------------------------------------------------------------

set -eo pipefail

THIS=${0##*/}
RUN_DIR=$(pwd -P)
PARTMESH_INPUT=${1:-"$RUN_DIR/partmesh.txt"}

info()
{
    printf '%s: INFO: %s\n' "$THIS" "$*"
}

warn()
{
    printf '%s: WARNING: %s\n' "$THIS" "$*" >&2
}

fatal()
{
    printf '%s: ERROR: %s\n' "$THIS" "$*" >&2
    exit 1
}

usage()
{
    cat <<EOF
Usage:
  $THIS
  $THIS /path/to/optimized-partmesh.txt

Run this from an existing ASGS scenario/run directory. With no argument,
the script uses ./partmesh.txt.
EOF
}

if [[ $# -gt 1 ]]; then
    usage >&2
    exit 2
fi

# Preserve values already resolved by the loaded ASGS shell/profile. Sourcing
# ASGS_CONFIG may set INSTANCENAME back to "auto", while the active shell may
# already contain the resolved instance name used by asgs_main.sh.
LOADED_INSTANCENAME=${INSTANCENAME:-}
LOADED_PREPPEDARCHIVE=${PREPPEDARCHIVE:-}
LOADED_RUNARCHIVEBASE=${RUNARCHIVEBASE:-}
LOADED_SCRIPTDIR=${SCRIPTDIR:-}
LOADED_ADCIRCDIR=${ADCIRCDIR:-}
LOADED_SCRATCH=${SCRATCH:-}
LOADED_NCPU=${NCPU:-}
LOADED_GRIDNAME=${GRIDNAME:-}

[[ -n ${ASGS_CONFIG:-} ]] ||
    fatal "ASGS_CONFIG is not set. Load the intended ASGS profile first."

[[ -r $ASGS_CONFIG ]] ||
    fatal "ASGS_CONFIG '$ASGS_CONFIG' does not exist or is not readable."

info "Loading ASGS configuration '$ASGS_CONFIG'."

# ASGS configuration files are Bash source files and may expect variables from
# the already-loaded ASGS profile. Do not enable nounset until after sourcing.
# shellcheck disable=SC1090
source "$ASGS_CONFIG"

set -u

# Prefer values resolved in the current ASGS shell when the config leaves the
# corresponding value empty, null, or auto.
SCRIPTDIR=${SCRIPTDIR:-$LOADED_SCRIPTDIR}
ADCIRCDIR=${ADCIRCDIR:-$LOADED_ADCIRCDIR}
SCRATCH=${SCRATCH:-$LOADED_SCRATCH}
NCPU=${NCPU:-$LOADED_NCPU}
GRIDNAME=${GRIDNAME:-$LOADED_GRIDNAME}

if [[ -z ${INSTANCENAME:-} || $INSTANCENAME == auto || $INSTANCENAME == null ]]; then
    if [[ -n $LOADED_INSTANCENAME &&
          $LOADED_INSTANCENAME != auto &&
          $LOADED_INSTANCENAME != null ]]; then
        INSTANCENAME=$LOADED_INSTANCENAME
    fi
fi

# Resolve INSTANCENAME exactly as asgs_main.sh does when it is still "auto".
if [[ -z ${INSTANCENAME:-} || $INSTANCENAME == auto || $INSTANCENAME == null ]]; then
    GET_INSTANCENAME=
    if command -v get-instancename >/dev/null 2>&1; then
        GET_INSTANCENAME=$(command -v get-instancename)
    elif [[ -n ${SCRIPTDIR:-} && -x $SCRIPTDIR/bin/get-instancename ]]; then
        GET_INSTANCENAME=$SCRIPTDIR/bin/get-instancename
    fi

    [[ -n $GET_INSTANCENAME ]] ||
        fatal "INSTANCENAME is '$INSTANCENAME', but get-instancename was not found."

    for required in GRIDNAME TROPICALCYCLONE BACKGROUNDMET STORM YEAR; do
        [[ -n ${!required:-} && ${!required} != null ]] ||
            fatal "Cannot resolve INSTANCENAME because '$required' is not set."
    done

    INSTANCENAME=$(
        "$GET_INSTANCENAME" \
            "$GRIDNAME" \
            "$TROPICALCYCLONE" \
            "$BACKGROUNDMET" \
            "$STORM" \
            "$YEAR"
    )
fi

for required in GRIDNAME INSTANCENAME NCPU SCRATCH; do
    [[ -n ${!required:-} && ${!required} != null ]] ||
        fatal "Required ASGS value '$required' is not set."
done

[[ $NCPU =~ ^[1-9][0-9]*$ ]] ||
    fatal "NCPU must be a positive integer; found '$NCPU'."

[[ $SCRATCH == /* ]] ||
    fatal "SCRATCH must be an absolute path; found '$SCRATCH'."

# asgs_main.sh derives this name after loading ASGS_CONFIG. Prefer an already
# resolved value from the active ASGS environment; otherwise derive it here.
if [[ -n $LOADED_PREPPEDARCHIVE && $LOADED_PREPPEDARCHIVE != null ]]; then
    PREPPEDARCHIVE=$LOADED_PREPPEDARCHIVE
else
    PREPPEDARCHIVE="prepped_${GRIDNAME}_${INSTANCENAME}_${NCPU}.tar.gz"
fi

# RUNARCHIVEBASE is presently set to SCRATCH by asgs_main.sh. Honor an already
# resolved value, but fall back to SCRATCH.
if [[ -n $LOADED_RUNARCHIVEBASE && $LOADED_RUNARCHIVEBASE != null ]]; then
    RUNARCHIVEBASE=$LOADED_RUNARCHIVEBASE
else
    RUNARCHIVEBASE=$SCRATCH
fi

[[ $RUNARCHIVEBASE == /* ]] ||
    fatal "RUNARCHIVEBASE must be an absolute path; found '$RUNARCHIVEBASE'."

if [[ $PARTMESH_INPUT != /* ]]; then
    PARTMESH_INPUT=$RUN_DIR/$PARTMESH_INPUT
fi

[[ -s $PARTMESH_INPUT ]] ||
    fatal "Optimized partmesh file '$PARTMESH_INPUT' was not found or is empty."

[[ -e $RUN_DIR/fort.14 ]] ||
    fatal "Run directory '$RUN_DIR' does not contain fort.14."

[[ -e $RUN_DIR/fort.15 ]] ||
    fatal "Run directory '$RUN_DIR' does not contain fort.15."

mkdir -p "$RUNARCHIVEBASE"
[[ -w $RUNARCHIVEBASE ]] ||
    fatal "Archive directory '$RUNARCHIVEBASE' is not writable."

ARCHIVE_PATH=$RUNARCHIVEBASE/$PREPPEDARCHIVE
TIMESTAMP=$(date +'%Y%m%dT%H%M%S%z')
LOGFILE=$RUN_DIR/update-prepped-archive.$TIMESTAMP.log
WORK_DIR=$(mktemp -d "$RUN_DIR/.update-prepped-archive.XXXXXX")
TMP_ARCHIVE=$(mktemp "$RUNARCHIVEBASE/.${PREPPEDARCHIVE}.new.XXXXXX")
LOCKFILE=$RUNARCHIVEBASE/.${PREPPEDARCHIVE}.lock
SUCCESS=no

cleanup()
{
    local rc=$?

    rm -f "$TMP_ARCHIVE"

    if [[ $SUCCESS == yes ]]; then
        rm -rf "$WORK_DIR"
    else
        warn "The update did not complete."
        warn "Temporary work directory retained at '$WORK_DIR'."
        warn "Log retained at '$LOGFILE'."
    fi

    exit "$rc"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

if command -v flock >/dev/null 2>&1; then
    exec 9>"$LOCKFILE"
    flock -n 9 ||
        fatal "Another process is updating '$ARCHIVE_PATH'."
else
    warn "'flock' was not found; continuing without an archive update lock."
fi

# Determine the full-domain node count from line 2 of fort.14:
#     NE NP
read -r ELEMENT_COUNT NODE_COUNT _ < <(sed -n '2p' "$RUN_DIR/fort.14")

[[ ${ELEMENT_COUNT:-} =~ ^[1-9][0-9]*$ &&
   ${NODE_COUNT:-} =~ ^[1-9][0-9]*$ ]] ||
    fatal "Could not read valid NE and NP values from line 2 of fort.14."

info "Run directory       : $RUN_DIR"
info "Grid name           : $GRIDNAME"
info "Instance name       : $INSTANCENAME"
info "Compute subdomains  : $NCPU"
info "Full-domain nodes   : $NODE_COUNT"
info "Partmesh source     : $PARTMESH_INPUT"
info "Forecast archive    : $ARCHIVE_PATH"
info "Temporary work dir  : $WORK_DIR"
info "Log file            : $LOGFILE"

# partmesh.txt contains one 1-based subdomain number per full-domain node.
awk -v expected_nodes="$NODE_COUNT" -v ncpu="$NCPU" '
    BEGIN {
        failed = 0
    }
    NF != 1 || $1 !~ /^[0-9]+$/ {
        printf("Invalid partmesh.txt record at line %d: expected one integer, found [%s]\n",
               NR, $0) > "/dev/stderr"
        failed = 1
        next
    }
    $1 < 1 || $1 > ncpu {
        printf("Invalid partition number at line %d: %d is outside 1..%d\n",
               NR, $1, ncpu) > "/dev/stderr"
        failed = 1
    }
    {
        seen[$1] = 1
    }
    END {
        if (NR != expected_nodes) {
            printf("partmesh.txt has %d records; fort.14 contains %d nodes\n",
                   NR, expected_nodes) > "/dev/stderr"
            failed = 1
        }
        for (i = 1; i <= ncpu; i++) {
            if (!(i in seen)) {
                printf("partmesh.txt does not assign any node to partition %d\n",
                       i) > "/dev/stderr"
                failed = 1
            }
        }
        exit failed
    }
' "$PARTMESH_INPUT" ||
    fatal "Optimized partmesh.txt failed validation."

SOURCE_PARTMESH_SHA=$(sha256sum "$PARTMESH_INPUT" | awk '{print $1}')
info "Optimized partmesh SHA-256: $SOURCE_PARTMESH_SHA"

# Copy the optimized partition assignment. Link the full-domain ADCIRC inputs
# from the run directory so prepall operates in isolation and cannot overwrite
# PE files or results in the existing run.
cp -- "$PARTMESH_INPUT" "$WORK_DIR/partmesh.txt"

while IFS= read -r -d '' input_file; do
    base=${input_file##*/}
    ln -s "$input_file" "$WORK_DIR/$base"
done < <(
    find "$RUN_DIR" -maxdepth 1 \
        \( -type f -o -type l \) \
        \( -name 'fort.*' -o \
           -name 'swaninit' -o \
           -name 'time-bonnet.in' \) \
        -print0
)

# Locate adcprep from the ADCIRC installation selected by the loaded profile.
declare -a ADCPREP_COMMAND
if [[ -n ${ADCIRCDIR:-} &&
      $ADCIRCDIR != null &&
      -x $ADCIRCDIR/adcprep ]]; then
    ADCPREP_COMMAND=("$ADCIRCDIR/adcprep")
elif command -v adcprep >/dev/null 2>&1; then
    ADCPREP_COMMAND=("$(command -v adcprep)")
else
    fatal "adcprep was not found under ADCIRCDIR='$ADCIRCDIR' or in PATH."
fi

info "Running: ${ADCPREP_COMMAND[*]} --np $NCPU --prepall --strict-boundaries"

# Cephas and other ASGS platforms commonly require an unlimited stack for
# ADCIRC utilities as well as for the model itself.
ulimit -s unlimited

(
    cd "$WORK_DIR"
    "${ADCPREP_COMMAND[@]}" \
        --np "$NCPU" \
        --prepall \
        --strict-boundaries
) 2>&1 | tee "$LOGFILE"

STAGED_PARTMESH_SHA=$(sha256sum "$WORK_DIR/partmesh.txt" | awk '{print $1}')
[[ $STAGED_PARTMESH_SHA == "$SOURCE_PARTMESH_SHA" ]] ||
    fatal "adcprep changed partmesh.txt unexpectedly."

mapfile -t PE_DIRS < <(
    find "$WORK_DIR" \
        -mindepth 1 \
        -maxdepth 1 \
        -type d \
        -name 'PE[0-9]*' \
        -printf '%f\n' |
    LC_ALL=C sort
)

[[ ${#PE_DIRS[@]} -eq $NCPU ]] ||
    fatal "adcprep created ${#PE_DIRS[@]} PE directories; expected $NCPU."

INCLUDE_FORT24=no
if [[ ${selfAttractionEarthLoadTide:-notprovided} != notprovided &&
      ${selfAttractionEarthLoadTide:-null} != null ]]; then
    INCLUDE_FORT24=yes
fi

MANIFEST=$WORK_DIR/prepped-archive.manifest
printf '%s\n' partmesh.txt > "$MANIFEST"

for pe_dir in "${PE_DIRS[@]}"; do
    [[ -s $WORK_DIR/$pe_dir/fort.14 ]] ||
        fatal "Required file '$pe_dir/fort.14' is missing or empty."

    [[ -s $WORK_DIR/$pe_dir/fort.18 ]] ||
        fatal "Required file '$pe_dir/fort.18' is missing or empty."

    printf '%s\n' \
        "$pe_dir/fort.14" \
        "$pe_dir/fort.18" >> "$MANIFEST"

    if [[ $INCLUDE_FORT24 == yes ]]; then
        [[ -s $WORK_DIR/$pe_dir/fort.24 ]] ||
            fatal "Self-attraction/earth-load tide is enabled, but '$pe_dir/fort.24' is missing or empty."

        printf '%s\n' "$pe_dir/fort.24" >> "$MANIFEST"
    fi
done

info "Creating replacement archive."

tar -C "$WORK_DIR" \
    -czf "$TMP_ARCHIVE" \
    -T "$MANIFEST"

tar -tzf "$TMP_ARCHIVE" >/dev/null ||
    fatal "The newly created archive failed gzip/tar validation."

NEW_ARCHIVE_SHA=$(sha256sum "$TMP_ARCHIVE" | awk '{print $1}')
BACKUP_PATH=

if [[ -e $ARCHIVE_PATH ]]; then
    OLD_PARTMESH_SHA=$(
        tar -xOzf "$ARCHIVE_PATH" partmesh.txt 2>/dev/null |
        sha256sum |
        awk '{print $1}'
    )

    info "Existing archive partmesh SHA-256: $OLD_PARTMESH_SHA"

    BACKUP_PATH=$ARCHIVE_PATH.before-$TIMESTAMP

    # A hard link provides an immediate, space-efficient backup on the same
    # filesystem. Fall back to a normal/reflink copy if hard linking is not
    # supported by the underlying filesystem.
    if ! ln -- "$ARCHIVE_PATH" "$BACKUP_PATH" 2>/dev/null; then
        cp --reflink=auto --preserve=mode,timestamps \
            "$ARCHIVE_PATH" "$BACKUP_PATH"
    fi

    chmod --reference="$ARCHIVE_PATH" "$TMP_ARCHIVE"
    chgrp --reference="$ARCHIVE_PATH" "$TMP_ARCHIVE" 2>/dev/null || true
else
    chmod 0664 "$TMP_ARCHIVE"
fi

# TMP_ARCHIVE is created in RUNARCHIVEBASE, so this rename replaces the
# archive atomically on the same filesystem.
mv -f -- "$TMP_ARCHIVE" "$ARCHIVE_PATH"

FINAL_ARCHIVE_SHA=$(sha256sum "$ARCHIVE_PATH" | awk '{print $1}')
[[ $FINAL_ARCHIVE_SHA == "$NEW_ARCHIVE_SHA" ]] ||
    fatal "Archive checksum changed while installing '$ARCHIVE_PATH'."

SUCCESS=yes

info "Forecast preprocessing archive updated successfully."
info "Installed archive SHA-256: $FINAL_ARCHIVE_SHA"
if [[ -n $BACKUP_PATH ]]; then
    info "Previous archive backup : $BACKUP_PATH"
fi
info "Future scenarios that have not yet been prepped will use this archive."

