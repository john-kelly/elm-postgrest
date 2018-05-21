source ./.env \
    && dropdb $DB_NAME --if-exists \
    && createdb $DB_NAME \
    && psql --dbname=$DB_NAME --file=./init.sql
