source ./.env \
    && dropdb $DB_NAME \
    && createdb $DB_NAME \
    && psql --dbname=$DB_NAME --file=./init.sql
