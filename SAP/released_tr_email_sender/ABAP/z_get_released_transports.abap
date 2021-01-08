FUNCTION Z_GET_RELEASED_TRANSPORTS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(AGE_IN_DAYS) TYPE  I
*"  EXPORTING
*"     VALUE(JSON_DATA) TYPE  STRING
*"----------------------------------------------------------------------
  " calculate oldest date timestamp
  DATA(oldest_date_datum) = sy-datum - age_in_days.
  CONVERT DATE oldest_date_datum INTO TIME STAMP DATA(oldest_date_timestamp) TIME ZONE 'UTC'.
  " convert to strgin for SQL argument pass
  data(oldest_date_string) = |{ oldest_date_timestamp }|.

  " get all TRs owned by me
  SELECT e070~trkorr AS tr_number,
         e070a~reference AS export_datetime,
         e070~as4user AS owner,
         e07t~as4text AS description
    FROM  e070a
    INNER JOIN e070 ON e070a~trkorr = e070~trkorr
    INNER JOIN e07t ON e07t~trkorr = e070~trkorr
    INTO TABLE @DATA(released_transports)
    WHERE e070a~attribute EQ 'EXPORT_TIMESTAMP'
      AND e070a~reference GE @oldest_date_string
      AND e070~trstatus EQ 'R'
      AND e070~as4user EQ @sy-uname
      AND e07t~langu EQ 'E'.


  json_data = /ui2/cl_json=>serialize( data = released_transports ).


ENDFUNCTION.
