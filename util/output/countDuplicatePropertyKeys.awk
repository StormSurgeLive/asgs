# check for duplicate property keys in the run.properties file (trimmed keys)
BEGIN { FS=":" }
{
    key = $1
    gsub(/^[ \t]+|[ \t]+$/, "", key)  # trim leading/trailing whitespace
    if (a[key]++) dup++
}
END { printf "%d", dup }