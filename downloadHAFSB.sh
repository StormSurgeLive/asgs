#!/usr/bin/env bash
set -euo pipefail

# downloadHAFSB.sh
#
# Download NOAA/NCEP operational HAFS-B data for North Atlantic basin storms.
#
# NOAA NOMADS layout:
#   https://nomads.ncep.noaa.gov/pub/data/nccf/com/hafs/prod/
#     hfsb.YYYYMMDD/HH/
#
# HAFS storm IDs use ATCF basin suffixes. North Atlantic = "l", e.g. 05l.
#
# Default behavior:
#   - date: current UTC date
#   - cycle: latest available cycle for that date
#   - storm: all available North Atlantic storms in that cycle
#   - product: storm.atm
#   - forecast hours: 000 through 126 every 3 hours
#   - downloads both .grb2 and .idx
#
# Examples:
#   ./downloadHAFSB.sh
#   ./downloadHAFSB.sh -d 20260810 -c 06
# For a specific storm and cycle:
#   ./downloadHAFSB.sh -d 20260810 -c 06 -s 05l
# To download the larger parent-domain atmospheric files only through forecast hour 48:
#   ./downloadHAFSB.sh -s 05l -p parent.atm -b 0 -e 48
#   ./downloadHAFSB.sh -s 05l -p storm.atm -b 0 -e 126 -i 3
# Inspect what's available without downloading anything:
#   ./downloadHAFSB.sh --list
#
# Requirements:
#   bash, curl, grep, sed, awk, sort
#------------------------------------------------------------------------------
# NOAA currently organizes HAFS-B data under directories like hfsb.YYYYMMDD/HH/;
# the live archive exposes cycles such as 00 and 06, with HAFS-B GRIB2 products
# including storm.atm, parent.atm, parent.sat, and swath files.
#
# The atmospheric products shown by NOAA are produced at 3-hour intervals
# (f000, f003, f006, …) through at least f126.
#
# By default, it uses today's UTC date, automatically finds the latest available
# HAFS-B cycle, detects all North Atlantic storms/invests, and downloads
# storm.atm GRIB2 files from f000 through f126 every 3 hours, along with their
# .idx inventories.
#
# Scrapes NOAA's actual cycle inventory before downloading, rather than
# assuming a storm exists. If there is no Atlantic storm or invest being run
# by HAFS-B for that cycle, it exits cleanly.
#------------------------------------------------------------------------------

BASE_URL="https://nomads.ncep.noaa.gov/pub/data/nccf/com/hafs/prod"

DATE_UTC="$(date -u +%Y%m%d)"
CYCLE=""
STORM=""
PRODUCT="storm.atm"
FH_START=0
FH_END=126
FH_INC=3
OUTDIR="hafsb"
LIST_ONLY=0
GET_IDX=1
OVERWRITE=0

usage() {
    cat <<'EOF'
Usage:
  downloadHAFSB.sh [options]

Options:
  -d YYYYMMDD   Model date in UTC.
                Default: current UTC date.

  -c HH         Model cycle: 00, 06, 12, or 18.
                Default: latest available cycle for the requested date.

  -s STORM      Atlantic ATCF storm ID, e.g. 05l or 05L.
                Default: download all North Atlantic storms found.

  -p PRODUCT    HAFS-B product:
                  storm.atm
                  parent.atm
                  parent.sat
                Default: storm.atm

  -b HOUR       First forecast hour.
                Default: 0

  -e HOUR       Last forecast hour.
                Default: 126

  -i HOURS      Forecast-hour increment.
                Default: 3

  -o DIR        Output root directory.
                Default: hafsb

  --no-idx      Do not download GRIB2 .idx inventory files.

  --overwrite   Re-download files that already exist.

  --list        List available North Atlantic HAFS-B files/storms only.

  -h, --help    Show this help.

Examples:
  # Latest HAFS-B cycle and all Atlantic storms
  ./downloadHAFSB.sh

  # Specific date/cycle
  ./downloadHAFSB.sh -d 20260810 -c 06

  # Specific Atlantic storm
  ./downloadHAFSB.sh -d 20260810 -c 06 -s 05l

  # Parent atmospheric GRIB2, hours 0-48
  ./downloadHAFSB.sh -s 05l -p parent.atm -b 0 -e 48

  # See what's available without downloading
  ./downloadHAFSB.sh -d 20260810 -c 06 --list
EOF
}

need_cmd() {
    command -v "$1" >/dev/null 2>&1 || {
        echo "ERROR: required command not found: $1" >&2
        exit 1
    }
}

for cmd in curl grep sed awk sort; do
    need_cmd "$cmd"
done

while [[ $# -gt 0 ]]; do
    case "$1" in
        -d)
            DATE_UTC="${2:?Missing value for -d}"
            shift 2
            ;;
        -c)
            CYCLE="${2:?Missing value for -c}"
            shift 2
            ;;
        -s)
            STORM="${2:?Missing value for -s}"
            shift 2
            ;;
        -p)
            PRODUCT="${2:?Missing value for -p}"
            shift 2
            ;;
        -b)
            FH_START="${2:?Missing value for -b}"
            shift 2
            ;;
        -e)
            FH_END="${2:?Missing value for -e}"
            shift 2
            ;;
        -i)
            FH_INC="${2:?Missing value for -i}"
            shift 2
            ;;
        -o)
            OUTDIR="${2:?Missing value for -o}"
            shift 2
            ;;
        --no-idx)
            GET_IDX=0
            shift
            ;;
        --overwrite)
            OVERWRITE=1
            shift
            ;;
        --list)
            LIST_ONLY=1
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "ERROR: unknown option: $1" >&2
            usage >&2
            exit 1
            ;;
    esac
done

# ---------- Validate input ----------

if [[ ! "$DATE_UTC" =~ ^[0-9]{8}$ ]]; then
    echo "ERROR: date must be YYYYMMDD: $DATE_UTC" >&2
    exit 1
fi

if [[ -n "$CYCLE" && ! "$CYCLE" =~ ^(00|06|12|18)$ ]]; then
    echo "ERROR: cycle must be 00, 06, 12, or 18: $CYCLE" >&2
    exit 1
fi

case "$PRODUCT" in
    storm.atm|parent.atm|parent.sat)
        ;;
    *)
        echo "ERROR: unsupported product: $PRODUCT" >&2
        echo "Use storm.atm, parent.atm, or parent.sat." >&2
        exit 1
        ;;
esac

if ! [[ "$FH_START" =~ ^[0-9]+$ &&
        "$FH_END" =~ ^[0-9]+$ &&
        "$FH_INC" =~ ^[0-9]+$ ]]; then
    echo "ERROR: forecast hours must be non-negative integers." >&2
    exit 1
fi

if (( FH_INC == 0 )); then
    echo "ERROR: forecast increment cannot be zero." >&2
    exit 1
fi

if (( FH_END < FH_START )); then
    echo "ERROR: end forecast hour must be >= start hour." >&2
    exit 1
fi

if [[ -n "$STORM" ]]; then
    STORM="$(printf '%s' "$STORM" | tr '[:upper:]' '[:lower:]')"
    if [[ ! "$STORM" =~ ^[0-9]{2}l$ ]]; then
        echo "ERROR: North Atlantic storm ID must look like 01l, 05l, 90l, etc." >&2
        exit 1
    fi
fi

DATE_URL="${BASE_URL}/hfsb.${DATE_UTC}"

# ---------- Determine latest available cycle ----------

if [[ -z "$CYCLE" ]]; then
    echo "Checking HAFS-B cycles for ${DATE_UTC} ..."

    PAGE="$(curl -fsSL --retry 3 --retry-delay 3 "${DATE_URL}/" || true)"

    if [[ -z "$PAGE" ]]; then
        echo "ERROR: HAFS-B date directory not found or unavailable:" >&2
        echo "  ${DATE_URL}/" >&2
        exit 1
    fi

    CYCLE="$(
        printf '%s\n' "$PAGE" |
        grep -Eo 'href="(00|06|12|18)/"' |
        sed -E 's/.*"([0-9]{2})\/"/\1/' |
        sort -n |
        tail -1
    )"

    if [[ -z "$CYCLE" ]]; then
        echo "ERROR: no HAFS-B cycle directories found for ${DATE_UTC}." >&2
        exit 1
    fi

    echo "Using latest available cycle: ${CYCLE} UTC"
fi

CYCLE_URL="${DATE_URL}/${CYCLE}"

# ---------- Get directory inventory ----------

echo "Reading:"
echo "  ${CYCLE_URL}/"

PAGE="$(curl -fsSL --retry 3 --retry-delay 3 "${CYCLE_URL}/" || true)"

if [[ -z "$PAGE" ]]; then
    echo "ERROR: HAFS-B cycle directory not found or unavailable:" >&2
    echo "  ${CYCLE_URL}/" >&2
    exit 1
fi

# Extract all filenames from the HTML directory listing.
mapfile -t ALL_FILES < <(
    printf '%s\n' "$PAGE" |
    grep -Eo 'href="[^"]+"' |
    sed -E 's/^href="//; s/"$//' |
    grep -vE '^(\.\./|/|https?://)' |
    sort -u
)

# ---------- Find Atlantic storms ----------

mapfile -t ATL_STORMS < <(
    printf '%s\n' "${ALL_FILES[@]}" |
    grep -E '^[0-9]{2}l\.[0-9]{10}\.hfsb\.' |
    sed -E 's/^([0-9]{2}l)\..*/\1/' |
    sort -u
)

if [[ ${#ATL_STORMS[@]} -eq 0 ]]; then
    echo
    echo "No North Atlantic HAFS-B storm IDs were found for:"
    echo "  ${DATE_UTC} ${CYCLE} UTC"
    echo
    echo "This usually means HAFS-B did not have an Atlantic storm/invest"
    echo "running in this cycle, or the requested cycle is not yet populated."
    exit 0
fi

echo
echo "Available North Atlantic HAFS-B storms:"
printf '  %s\n' "${ATL_STORMS[@]}"

if [[ -n "$STORM" ]]; then
    found=0
    for s in "${ATL_STORMS[@]}"; do
        if [[ "$s" == "$STORM" ]]; then
            found=1
            break
        fi
    done

    if (( found == 0 )); then
        echo "ERROR: requested Atlantic storm ${STORM} is not available." >&2
        exit 1
    fi

    STORMS=("$STORM")
else
    STORMS=("${ATL_STORMS[@]}")
fi

# ---------- List mode ----------

if (( LIST_ONLY == 1 )); then
    echo
    for s in "${STORMS[@]}"; do
        echo "Files for ${s}:"
        printf '%s\n' "${ALL_FILES[@]}" |
        grep -E "^${s}\.${DATE_UTC}${CYCLE}\.hfsb\." |
        sed 's/^/  /'
        echo
    done
    exit 0
fi

# ---------- Download ----------

download_one() {
    local remote="$1"
    local localfile="$2"

    if [[ -s "$localfile" && "$OVERWRITE" -eq 0 ]]; then
        echo "EXISTS   $(basename "$localfile")"
        return 0
    fi

    echo "GET      $(basename "$localfile")"

    # --continue-at - allows restart of partial downloads.
    # --retry-all-errors improves resilience to temporary NOMADS failures.
    curl \
        -fL \
        --retry 5 \
        --retry-delay 5 \
        --retry-all-errors \
        --continue-at - \
        -o "$localfile" \
        "$remote"
}

for s in "${STORMS[@]}"; do
    TARGET_DIR="${OUTDIR}/${DATE_UTC}/${CYCLE}/${s}/${PRODUCT}"
    mkdir -p "$TARGET_DIR"

    echo
    echo "Downloading HAFS-B:"
    echo "  storm   : ${s}"
    echo "  date    : ${DATE_UTC}"
    echo "  cycle   : ${CYCLE} UTC"
    echo "  product : ${PRODUCT}"
    echo "  hours   : ${FH_START}-${FH_END} every ${FH_INC} h"
    echo "  output  : ${TARGET_DIR}"
    echo

    for (( fh=FH_START; fh<=FH_END; fh+=FH_INC )); do
        printf -v FHH "%03d" "$fh"

        FILE="${s}.${DATE_UTC}${CYCLE}.hfsb.${PRODUCT}.f${FHH}.grb2"

        # NOAA may not produce every product/hour for every run.
        # Check the cycle inventory before issuing the large download.
        if ! printf '%s\n' "${ALL_FILES[@]}" | grep -Fxq "$FILE"; then
            echo "MISSING  ${FILE}"
            continue
        fi

        download_one \
            "${CYCLE_URL}/${FILE}" \
            "${TARGET_DIR}/${FILE}"

        if (( GET_IDX == 1 )); then
            IDX="${FILE}.idx"
            if printf '%s\n' "${ALL_FILES[@]}" | grep -Fxq "$IDX"; then
                download_one \
                    "${CYCLE_URL}/${IDX}" \
                    "${TARGET_DIR}/${IDX}"
            fi
        fi
    done
done

echo
echo "HAFS-B download complete."
