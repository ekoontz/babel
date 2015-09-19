if [ ! ${DATABASE_URL} ]; then
    echo "you must define DATABASE_URL in your environment."
    exit 1
fi

#TODO: parse from DATABASE_URL
USER=$(echo ${DATABASE_URL} | sed 's/.*\/\/\([^@]*\).*/\1/g')
DB=$(echo $DATABASE_URL | sed 's/.*\/\/[^@]*@localhost\/\(.*\)/\1/g')

echo "\d" |  psql -U ${USER} ${DB} | grep expression_import_ | awk '{print $3}' | xargs -I{} echo "drop table {};" | psql -U ${USER} ${DB}


