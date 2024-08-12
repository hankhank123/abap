FUNCTION z_fi_test1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------


  DATA: lt_data TYPE TABLE OF zsc_vuelos.

  SELECT *
  FROM zsc_vuelos
  INTO TABLE @lt_data.


ENDFUNCTION.
