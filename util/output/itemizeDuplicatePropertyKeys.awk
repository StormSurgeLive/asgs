# create and return a list of duplicated property
# keys and how many times each one occurred
BEGIN { FS=":" }
{
    key = $1
    gsub(/^[ \t]+|[ \t]+$/, "", key)  # trim leading/trailing whitespace
    count[key]++
}
END {
        for (item in count)
           if (count[item] > 1)
               print item, count[item]
    }