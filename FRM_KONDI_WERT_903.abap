FORM FRM_KONDI_WERT_903.
* constante unidad tributaria y tarifa base
  CONSTANTS c_unit TYPE kschl VALUE 'UT' .
  CONSTANTS c_base TYPE kschl VALUE 'BASE' .
* constante tasa de cambio por defecto
  CONSTANTS c_rate TYPE tkurst VALUE 'M' .
* constante condición de cantidad
  CONSTANTS c_quan( 2) VALUE 'ZC'.
* valor de la unidad tributaria
  DATA lv_utval TYPE zutvalue.
* moneda local (base de cálculos)
  DATA lv_local_curr TYPE waers.
* tasa de cambio de 5 decimales
  DATA lv_exrate TYPE vvsbetrag.
* tasa de cambio de 2 decimales
  DATA lv_rate TYPE kawrt.
* variable de cálculo de %pro-rateo
  DATA lv_prrate TYPE kwert.
* importe de la posición
  DATA lv_kwert_pos TYPE kwert.
* importe total
  DATA lv_kwert_tot TYPE kwert.
* parámetros de configuración gastos oper.y logisticos
  DATA lt_gasto TYPE STANDARD TABLE OF ztmm_gastos_oyl.
  DATA ls_gasto LIKE LINE OF lt_gasto.
  DATA wa_gasto LIKE LINE OF lt_gasto.
* estructura de pase de condiciones y valores
  DATA lt_values TYPE STANDARD TABLE OF zmmconditionvalues.
  DATA ls_values LIKE LINE OF lt_values.
* parámetros para evaluación de formula
  DATA lv_value TYPE kawrt.
  DATA lv_subrc TYPE sy-subrc .
* tabla de valores de condición
  DATA wa_xkomv LIKE LINE OF xkomv.
  DATA ls_xkomv LIKE LINE OF xkomv.
  DATA ls_tkomv LIKE LINE OF tkomv.
* tablas de condiciones actualizadas
  DATA lt_xkomv LIKE xkomv[].
  DATA it_xkomv LIKE xkomv[].
* xkomv table index
  DATA lv_tabix LIKE sy-tabix .
* clase de condición y posición
  DATA lv_kschl TYPE kschl.
  DATA lv_kposn TYPE kposn.
* fecha de determinación de precios
  DATA lv_prsdt TYPE prsdt.

  CHECK konditionsart NE 'ZAGU'.
* CHECK tkomv[] IS NOT INITIAL.

  lv_prsdt = komk- prsdt.
  lv_kschl = xkomv- kschl.
  lv_kposn = xkomv- kposn.
  it_xkomv[] = xkomv[].

  READ TABLE it_xkomv INTO wa_xkomv
  WITH KEY kschl = lv_kschl.
  CHECK sy- subrc = 0 .
  lv_tabix = sy- tabix.

* valor de la unidad tributaria
  IMPORT lv_utval FROM MEMORY ID 'ZUT'.
* moneda de la unidad tributaria
  IMPORT lv_local_curr FROM MEMORY ID 'ZLC'.

  IF sy- subrc <> 0.
    CALL FUNCTION 'ZGET_UNIDAD_TRIBUTARIA'
      EXPORTING p_i_prsdt = lv_prsdt
      IMPORTING p_e_utvalue = lv_utval
                p_e_utwaers = lv_local_curr .
    IF lv_utval IS INITIAL .
*   "debe registrar el valor de la UT
    MESSAGE e005(zmm ) WITH lv_prsdt . EXIT.
    ELSE .
    EXPORT lv_utval TO MEMORY ID 'ZUT' .
    EXPORT lv_local_curr TO MEMORY ID 'ZLC' .
    ENDIF .
  ENDIF.

* obtiene configuración de gastos asociados
* a las condiciones de determinacion de precios
  IMPORT lt_gasto FROM MEMORY ID 'ZTG'.

  IF lt_gasto IS INITIAL .
    SELECT * INTO TABLE lt_gasto
      FROM ztmm_gastos_oyl
       FOR ALL ENTRIES IN it_xkomv
     WHERE kschl EQ it_xkomv -kschl
*    esta es la fecha para det.precios y tipo cambio
       AND begda LE lv_prsdt
       AND endda GE lv_prsdt .
    SORT lt_gasto BY kschl ASCENDING begda DESCENDING .
    DELETE ADJACENT DUPLICATES FROM lt_gasto COMPARING kschl .
    DELETE lt_gasto WHERE formula IS INITIAL.
    EXPORT lt_gasto TO MEMORY ID 'ZTG' .
  ENDIF.

  LOOP AT it_xkomv INTO wa_xkomv
    FROM lv_tabix
    WHERE kposn = lv_kposn .
*   obtiene la configuración del gasto
    READ TABLE lt_gasto INTO ls_gasto
    WITH KEY kschl = wa_xkomv -kschl.
    CHECK sy-subrc = 0.
*   "el concepto de gasto no está configurado
*   MESSAGE e004(zmm) WITH ls_xkomv-kschl.
*   EXIT.
    CLEAR : ls_values, lt_values.
*   valor de la tarifa
    ls_values- kschl = c_base .
    ls_values- value = ls_gasto -base.
    APPEND ls_values TO lt_values .
*   valor de la unidad tributaria actual
    ls_values- kschl = c_unit .
    ls_values- value =
    lv_utval * ls_gasto- utquan / 100.
    APPEND ls_values TO lt_values .

    lt_xkomv[] = it_xkomv[].
*   calcula el "pro-rateo" de la condición
    IF lv_prrate IS INITIAL .
*     obtiene importe de la posición
      READ TABLE tkomv INTO ls_tkomv
      WITH KEY
      kposn = lv_kposn
      stunr = '010'
      zaehk = '01'.
      lv_kwert_pos = ls_tkomv-kwert .
      lv_kwert_tot = 0.
*     obtiene el importe total
      LOOP AT tkomv INTO ls_tkomv
        WHERE stunr = '010'
          AND zaehk = '01' .
        lv_kwert_tot =
        lv_kwert_tot + ls_tkomv- kwert.
      ENDLOOP .
      CHECK lv_kwert_tot <> 0.
      lv_prrate =
      lv_kwert_pos * 100 /
      lv_kwert_tot.
    ENDIF .

*   determina los valores de cada condición en la formula
    LOOP AT lt_xkomv INTO ls_xkomv
      WHERE kposn EQ lv_kposn
        AND kschl IS NOT INITIAL.
*     verifica que la formula necesite de la cond.
      CHECK ls_gasto-formula CS ls_xkomv- kschl.
      ls_values- kschl = ls_xkomv -kschl.
*     ¿es una condición de cantidad?
      IF ls_xkomv-kschl+0 (2) EQ c_quan.
*        multiplica por 100 porque kpein tiene 2 decimales
         ls_values- value = ls_xkomv -kpein * 100 .
      ELSE ."es una condición de importe
      ls_values- value = ls_xkomv -kwert.
*     lee la configuración Z de la condición
      READ TABLE lt_gasto INTO wa_gasto
      WITH KEY kschl = ls_xkomv -kschl.
      IF sy-subrc <> 0.
      wa_gasto- waers = ls_xkomv -waers.
      wa_gasto- tkurst = c_rate .
      ENDIF .
*     verifica si la moneda de cond. es la local
      IF lv_local_curr = ls_xkomv -waers.
         lv_exrate = 10000. "esto es = 1.0000
      ELSE . "busca tasa de cambio de condición
        CALL FUNCTION 'READ_EXCHANGE_RATE'
          EXPORTING
            date             = lv_prsdt
            local_currency   = lv_local_curr
            foreign_currency = ls_xkomv -waers
            type_of_rate     = wa_gasto -tkurst
          IMPORTING
            exchange_rate    = lv_exrate
          EXCEPTIONS
            others           = 1 .
        IF sy-subrc <> 0 OR lv_exrate IS INITIAL.
          MESSAGE e006(zmm ) WITH "tasa de cambio USD/VEF no está configurada
          wa_gasto- tkurst lv_local_curr ls_xkomv-waers lv_prsdt .
          EXIT .
        ENDIF .
      ENDIF .
*     tasa de cambio con 2 decimales
      lv_rate = lv_exrate / 1000.
      ls_values- value =
      ls_xkomv- kwert * lv_rate.
      ls_values- value = ls_values -value / 100.
      ENDIF .
      APPEND ls_values TO lt_values .
    ENDLOOP .
*   determina el valor de la condición xkomv-kschl
    CHECK lt_values IS NOT INITIAL.
    CALL FUNCTION 'ZEVAL_FORMULA'
      EXPORTING formula = ls_gasto -formula
      IMPORTING value   = lv_value
                retcode = lv_subrc
      TABLES t_i_coefficients = lt_values .
    IF lv_subrc <> 0 .
*   "fórmula de cálculo de concepto de gasto errónea
    MESSAGE e007(zmm ) WITH ls_gasto -kschl lv_prsdt.
    ELSEIF wa_xkomv-waers EQ lv_local_curr.
      lv_exrate = 100000. "esto es = 1.0000
    ELSE ." busca la tasa de cambio de la condición calculada
      CALL FUNCTION 'READ_EXCHANGE_RATE'
        EXPORTING
          date             = lv_prsdt
          local_currency   = lv_local_curr
          foreign_currency = wa_xkomv -waers "ls_gasto-waers
          type_of_rate     = ls_gasto -tkurst
        IMPORTING
          exchange_rate    = lv_exrate
        EXCEPTIONS
          others           = 1.
      IF sy-subrc <> 0 OR lv_exrate IS INITIAL.
        MESSAGE e006(zmm ) WITH "tasa de cambio USD/VEF no está configurada
        ls_gasto- tkurst lv_local_curr wa_xkomv-waers lv_prsdt .
        EXIT .
      ENDIF .
    ENDIF .
*   tasa de cambio con 2 decimales
    lv_rate = lv_exrate / 1000.
    wa_xkomv- kwert = lv_value * 100 / lv_rate.
    wa_xkomv- kkurs = lv_exrate .
*   hace el pro-rateo del importe
    wa_xkomv- kwert =
    wa_xkomv- kwert * lv_prrate / 100.
    MODIFY it_xkomv FROM wa_xkomv .
  ENDLOOP.

* actualiza la tabla xkomv
* y su workarea
  xkomv[] = it_xkomv[].
  READ TABLE xkomv
  WITH KEY
  kposn = lv_kposn
  kschl = lv_kschl.

  DELETE xkomv
  WHERE kwert IS INITIAL
    AND kpein IS INITIAL
    AND kschl(1 ) = 'Z'.

ENDFORM.
