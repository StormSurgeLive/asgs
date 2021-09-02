#/bin/awk
# replaceURL.awk: populate an URL field in json file
# the values of u and i are supplied on the command line
{ 
if ($1~"hook.status.url") 
    print $1" "$2" "u"," ; 
else if ($1~"asgs.instance.status.url")  
    print $1" "$2" "i"," ; 
else 
    print $0 
}
