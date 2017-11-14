*&---------------------------------------------------------------------*
*& Report  ZLAT_FI_PG_RETENCIONIVA
*&
*&---------------------------------------------------------------------*
*& Descripcion: Comprobante de Retención de IVA
*& Autor      : José Sosa - LatCapital de Venezuela
*& Fecha      : 27/09/2013
*&---------------------------------------------------------------------*

REPORT  zlat_fi_pg_0003.

************************************************************************
*                            T A B L A S                               *
************************************************************************

TABLES : bkpf,
         vbrk,
         bset,
         bseg,
         lfa1,
         with_item,
         itcpo,
         bsak,
*         zretencion_iva,
         adrc.

************************************************************************
* Declaración de Variables del ALV
************************************************************************
*Variables globales:
INCLUDE zlat_fi_top_global.

TYPE-POOLS  slis.

DATA:
  ls_layout   TYPE slis_layout_alv,
  ls_fieldcat TYPE slis_fieldcat_alv,
  lt_fieldcat TYPE slis_t_fieldcat_alv," Field catalog
  ls_sort     TYPE slis_sortinfo_alv,
  lt_sort     TYPE slis_t_sortinfo_alv," Sort table
  lt_events   TYPE slis_t_event,
  ls_event    TYPE slis_alv_event.

DATA zrepid     LIKE sy-repid.
DATA fieldcat   TYPE slis_t_fieldcat_alv.
DATA w_fieldcat LIKE LINE OF lt_fieldcat.
DATA tfieldcat  TYPE slis_t_fieldcat_alv OCCURS 0.
DATA finsort    TYPE slis_t_sortinfo_alv.
DATA t_sort     TYPE slis_sortinfo_alv.
DATA tstatusset TYPE slis_formname.
DATA lc_glay    TYPE lvc_s_glay.

* Constants
CONSTANTS: ztop     TYPE slis_formname VALUE 'HEADER',
           zcommand TYPE slis_formname VALUE 'COMMANDS'.

************************************************************************
*                       V A R I A B L E S                              *
************************************************************************

DATA: adrnr1       LIKE t001-adrnr,
      name1(80), ""        LIKE adrc-name1,
      name2        LIKE adrc-name2,
      street       LIKE adrc-street,
      str_suppl1   LIKE adrc-str_suppl1,
      city1        LIKE adrc-city1,
      rif          LIKE t001z-paval,
      linea        TYPE i,
      zoper(3)     TYPE p  ,
**NNUNEZ
      v_suppl2      TYPE adrc-str_suppl2,
      v_zuonr       TYPE dzuonr,
      v_sgtxt       TYPE bseg-sgtxt,
      v_bschl       TYPE bseg-bschl,
      v_mwskz       TYPE bseg-mwskz,
      v_qsatz       LIKE with_item-qsatz.

TYPES: BEGIN OF ty_descarga,
  paval     TYPE t001z-paval,
  imp(6)    TYPE c, "periodo impositivo
  v_bldat(10)   TYPE c,"bldat     TYPE bkpf-bldat,
  oper(1)   TYPE c, " (C = Compras , V= Ventas)
  tdoc(2)   TYPE c, " (01 = Factura , 02 = Nota de Debito ,
*                      03 = Notas de Credito)
  stcd1     TYPE lfa1-stcd1,
*  xblnr     TYPE bkpf-xblnr,
  xblnr     TYPE char36,
*  xblnrnd    TYPE char25,
*  xblnrnc    TYPE char25,
  bktxt     TYPE bkpf-bktxt,
*  zuonr     TYPE bseg-zuonr,
  wrbtr     TYPE bseg-wrbtr,
*  wt_qsshb  TYPE acwt_item-wt_qsshb,
*  wt_qbshb2 TYPE acwt_item-wt_qbshb,
  wrbtr2    TYPE wrbtr,
  wrbtr4    TYPE wrbtr,
  sgtxt     TYPE bseg-sgtxt,
  cont      TYPE zretencion_iva-cont,
  wrbtr1    TYPE wrbtr,
  qsatz     TYPE qsatz,
  cero      TYPE c,
  "Nro de Expediente
END OF ty_descarga.

DATA: it_descarga TYPE STANDARD TABLE OF ty_descarga,
      wa_descarga TYPE ty_descarga.

************************************************************************
*                         TABLAS INTERNAS                              *
************************************************************************

DATA: BEGIN OF ti_bsak OCCURS 0.
        INCLUDE STRUCTURE bsak.
DATA: END OF ti_bsak.

DATA: BEGIN OF ti_bsik OCCURS 0.
        INCLUDE STRUCTURE bsik.
DATA: END OF ti_bsik.

DATA: BEGIN OF ti_bsaktemp OCCURS 0.
        INCLUDE STRUCTURE bsak.
DATA: END OF ti_bsaktemp.

DATA: BEGIN OF ti_bkpf OCCURS 0.
        INCLUDE STRUCTURE bkpf.
DATA: END OF ti_bkpf.

DATA: BEGIN OF ti_bkpf2 OCCURS 0.
        INCLUDE STRUCTURE bkpf.
DATA: END OF ti_bkpf2.

DATA: BEGIN OF ti_bset OCCURS 0.
        INCLUDE STRUCTURE bset.
DATA: END OF ti_bset.

DATA: BEGIN OF ti_bset2 OCCURS 0.
        INCLUDE STRUCTURE bset.
DATA: END OF ti_bset2.

DATA: BEGIN OF ti_lfa1 OCCURS 0.
        INCLUDE STRUCTURE lfa1.
DATA: END OF ti_lfa1.

DATA: BEGIN OF ti_adrc OCCURS 0.
        INCLUDE STRUCTURE adrc.
DATA: END OF ti_adrc.

DATA ti_with_item TYPE STANDARD TABLE OF with_item WITH HEADER LINE.

*DATA: ti_salida TYPE STANDARD TABLE OF zretencion_iva WITH HEADER LINE.

DATA: BEGIN OF ti_salida OCCURS 0.
        INCLUDE STRUCTURE zretencion_iva.
DATA: ktosl TYPE bset-ktosl.
DATA: END OF ti_salida.

DATA: wa_salida LIKE ti_salida.

"tabla que agrupa por indicadores 35 y 45
DATA: BEGIN OF ti_total OCCURS 0,
      cont(14) TYPE c,
      belnr  TYPE belnr_d,
      wrbtr  TYPE wrbtr,
*      wrbtr1 TYPE wrbtr,
*      wrbtr2 TYPE wrbtr,
      wrbtr3 TYPE wrbtr,
*      wrbtr4 TYPE wrbtr,
      qsatz  TYPE qsatz,
      END OF ti_total.

DATA: wa_total LIKE ti_total.

*DATA ti_cont  TYPE STANDARD TABLE OF zretencion_iva WITH HEADER LINE.
DATA: ti_cont TYPE TABLE OF zretencion_iva WITH HEADER LINE.
**********
DATA: BEGIN OF excento_iva OCCURS 0,
  belnr  LIKE bkpf-belnr,
  wrbtr1 LIKE bseg-wrbtr,
      END OF excento_iva.
**********+

DATA: BEGIN OF rt_bkpf OCCURS 0.
        INCLUDE STRUCTURE bkpf.
DATA: END OF rt_bkpf.

DATA: rt_with_item TYPE TABLE OF with_item,
      lineas TYPE i.


**********************************************************************
DATA: t_filetable TYPE filetable,
          w_filetable LIKE file_table-filename,
          w_subrc     TYPE i.
DATA: w_filename TYPE string.
CONSTANTS: c_asc TYPE char10 VALUE 'ASC'.

CONSTANTS: c_title TYPE string VALUE 'Seleccione Archivo'.  "#EC NOTEXT

DATA: v_path TYPE string,
      v_file TYPE string.

DATA: BEGIN OF t_salida_tmp OCCURS 0,
       bukrs    TYPE bukrs,
       belnr    TYPE belnr_d,
       gjahr    TYPE gjahr,
       mwskz    TYPE mwskz,
       qsatz(5) TYPE c,
       wrbtr    TYPE wrbtr,
       wrbtr1   TYPE wrbtr,
       wrbtr2   TYPE wrbtr,
       wrbtr3   TYPE wrbtr,
       wrbtr4   TYPE wrbtr,
      END OF t_salida_tmp,
      w_salida_tmp LIKE t_salida_tmp.

DATA t_bkpf_1 LIKE bkpf OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF ti_salida_alv OCCURS 0,
       bukrs  LIKE zretencion_iva-bukrs,
       cont       LIKE zretencion_iva-cont,
       lifnr1   LIKE zretencion_iva-lifnr1,
       name1  LIKE zretencion_iva-name1,
       stcd_suj LIKE zretencion_iva-stcd_suj,
       lifnr  LIKE zretencion_iva-lifnr,
       icono(4) TYPE  c,
       belnr  LIKE zretencion_iva-belnr,
       gjahr  LIKE bkpf-gjahr,
       blart  LIKE zretencion_iva-blart,
       bldat  LIKE zretencion_iva-bldat,
       xblnr  LIKE zretencion_iva-xblnr,
       bktxt  LIKE zretencion_iva-bktxt,
       xblnrnd  LIKE zretencion_iva-xblnrnd,
       xblnrnc  LIKE zretencion_iva-xblnrnc,
       trans  LIKE zretencion_iva-trans,
       wrbtr  LIKE zretencion_iva-wrbtr,
       wrbtr1   LIKE zretencion_iva-wrbtr1,
       wrbtr2   LIKE zretencion_iva-wrbtr2,
       qsatz  LIKE zretencion_iva-qsatz,
       wrbtr3   LIKE zretencion_iva-wrbtr3,
       wrbtr4   LIKE zretencion_iva-wrbtr4,
       mwskz  LIKE zretencion_iva-mwskz,
       augdt  LIKE zretencion_iva-augdt,
      END OF ti_salida_alv.

************************************************************************
*                         P A R A M E T R O S                          *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK bloque1 WITH FRAME TITLE text-t01.

PARAMETER:      p_bukrs LIKE bkpf-bukrs OBLIGATORY,
                p_cert  TYPE char8.

SELECT-OPTIONS: p_lifnr FOR  lfa1-lifnr ,
                p_belnr FOR  bkpf-belnr,
                p_budat FOR  bkpf-budat OBLIGATORY NO-EXTENSION.

SELECTION-SCREEN END OF BLOCK bloque1.

SELECTION-SCREEN BEGIN OF BLOCK descarga WITH FRAME TITLE text-002.
*
PARAMETERS:     rb3 RADIOBUTTON GROUP rg1 DEFAULT 'X'
                                          USER-COMMAND radio,
                rb1 RADIOBUTTON GROUP rg1,
                rb4 RADIOBUTTON GROUP rg1,
                rb2 RADIOBUTTON GROUP rg1.

PARAMETERS : p_file LIKE rlgrap-filename MODIF ID fil.

SELECTION-SCREEN END OF BLOCK descarga.

* Macros
DEFINE m_fieldcat.
  ls_fieldcat-ref_tabname = &1.
  ls_fieldcat-fieldname   = &2.
  ls_fieldcat-do_sum      = &3.
  ls_fieldcat-checkbox    = &4.
  ls_fieldcat-col_pos     = &5.
  ls_fieldcat-no_out      = &6.
  ls_fieldcat-seltext_l   = &7.
  ls_fieldcat-outputlen   = &8.
  ls_fieldcat-seltext_s = ls_fieldcat-seltext_m =
                          ls_fieldcat-seltext_l.
  ls_fieldcat-ddictxt   = 'L'.
  append ls_fieldcat to lt_fieldcat.
  clear ls_fieldcat.
END-OF-DEFINITION.
DEFINE m_sort.
  add 1 to ls_sort-spos.
  ls_sort-fieldname = &1.
  ls_sort-group     = &2.
  ls_sort-subtot    = &3.
  ls_sort-up        = &4.
  append ls_sort to lt_sort.
  clear ls_sort.
END-OF-DEFINITION.

INCLUDE zlat_fi_email_alv.

INITIALIZATION.
  zrepid  = sy-repid.


AT SELECTION-SCREEN OUTPUT.

*  PERFORM bloquear_campos.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM abrir_archivo.



************************************************************************
*                         R U T I N A S                                *
************************************************************************

START-OF-SELECTION.
  PERFORM buscar_datos.
  PERFORM llenar_tabla.

  IF rb1 EQ 'X'. "Formulario de Comprobante de Retencion
    PERFORM depurar_documentos.
    PERFORM formulario.
  ELSEIF rb2 EQ 'X'. "Generacion de TXT
    PERFORM depurar_documentos.
    PERFORM llenar_tabla_desc.
    PERFORM descarga_txt.
  ELSEIF rb3 EQ 'X'.  " Listado
    PERFORM gen_report.
  ELSEIF rb4 EQ 'X'.  " Envio de Correo
    PERFORM depurar_documentos.
    PERFORM enviar_correos.
  ENDIF.

************************************************************************
*                         P E R F O R M                                *
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  BUSCAR_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM buscar_datos.

  DATA v_tabix LIKE syst-tabix.

**** BUSCAR DATOS DE SOCIEDAD

  SELECT SINGLE adrnr FROM t001 INTO adrnr1
      WHERE bukrs EQ p_bukrs.

  SELECT SINGLE name1 name2 street str_suppl1 city1 str_suppl2
          FROM adrc
         INTO  (name1, name2, street, str_suppl1, city1, v_suppl2)
         WHERE addrnumber EQ adrnr1.

  CONCATENATE name1 name2 INTO name1 SEPARATED BY space.



* Rif del agente
  SELECT SINGLE paval FROM t001z INTO rif WHERE
    bukrs EQ p_bukrs AND party EQ gc_party.


**** BUSCAR DOCUMENTOS

  SELECT * FROM bkpf INTO TABLE rt_bkpf
  WHERE bukrs EQ p_bukrs
        AND   belnr IN p_belnr
        AND   budat IN p_budat
        AND   blart IN ('KR','RE','KG','KZ','KN').
  "******************* Se agrego KZ
*        AND   stblg EQ space.  " JSOSA 14.12.2011

  DESCRIBE TABLE rt_bkpf LINES linea.

**** BUSCAR POSICION DE DOCUMENTOS

  IF linea GT 0.

    PERFORM seleccionar_documentos.

    SELECT * FROM bsak INTO TABLE ti_bsak
      FOR ALL ENTRIES IN rt_bkpf
      WHERE   bukrs EQ p_bukrs
        AND   belnr EQ rt_bkpf-belnr
        AND   gjahr EQ rt_bkpf-gjahr
        AND   lifnr IN p_lifnr.

    SELECT * FROM bsik APPENDING TABLE ti_bsak
      FOR ALL ENTRIES IN rt_bkpf
      WHERE   bukrs EQ p_bukrs
        AND   belnr EQ rt_bkpf-belnr
        AND   gjahr EQ rt_bkpf-gjahr
        AND   lifnr IN p_lifnr.

    DESCRIBE TABLE ti_bsak LINES linea.

    IF linea GT 0.

**** BUSCAR ACREEDORES

      ti_bsaktemp[] = ti_bsak[].
      SORT ti_bsaktemp BY lifnr.
      DELETE ADJACENT DUPLICATES FROM ti_bsaktemp COMPARING lifnr.
      DELETE ti_bsaktemp WHERE lifnr NOT IN p_lifnr.
      DELETE ti_bsak     WHERE lifnr NOT IN p_lifnr.

      SELECT * FROM lfa1 INTO TABLE ti_lfa1
        FOR ALL ENTRIES IN ti_bsaktemp
        WHERE lifnr EQ ti_bsaktemp-lifnr.

      SELECT * FROM adrc INTO TABLE ti_adrc
        FOR ALL ENTRIES IN ti_lfa1
        WHERE addrnumber EQ ti_lfa1-adrnr.

**** BUSCAR DATOS DE CABECERA DE DOCUMENTOS

      ti_bkpf[] = rt_bkpf[].

* Borra de la tabla principal del ciclo todos los documentos
* que no pertenecen al Acreedor
      LOOP AT ti_bkpf.
        MOVE syst-tabix TO v_tabix.
        READ TABLE ti_bsak WITH KEY belnr = ti_bkpf-belnr
                                    gjahr = ti_bkpf-gjahr
                                    bukrs = ti_bkpf-bukrs.
        CHECK syst-subrc NE 0.
        DELETE ti_bkpf INDEX v_tabix.
      ENDLOOP.

**** BUSCAR DATOS DE RETENCION DE DOCUMENTOS

      SELECT * FROM with_item INTO TABLE ti_with_item
        FOR ALL ENTRIES IN ti_bsak
        WHERE   bukrs  EQ ti_bsak-bukrs
        AND     belnr  EQ ti_bsak-belnr
        AND     gjahr  EQ ti_bsak-gjahr    " insert 123
        AND     witht  IN ('VI','VT').

      IF p_cert IS NOT INITIAL.
        DELETE ti_with_item WHERE ctnumber+2(8) NE p_cert.
      ENDIF.

**** BUSCAR DATOS DE IMPUESTO DE DOCUMENTOS

      SELECT * FROM bset INTO TABLE ti_bset
        FOR ALL ENTRIES IN ti_bkpf
        WHERE bukrs EQ p_bukrs
        AND   belnr EQ ti_bkpf-belnr
        AND   gjahr EQ ti_bkpf-gjahr.

    ENDIF.
  ELSE.
    MESSAGE i208(00) WITH 'No existen Datos para esta Selección'.

    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    "BUSCAR_DATOS

*&---------------------------------------------------------------------*
*&      Form  LLENAR_TABLA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM llenar_tabla.
  DATA: contador(5) TYPE n,
        ld_tabix TYPE sy-tabix.
  DESCRIBE TABLE ti_bkpf LINES linea.

  LOOP AT ti_bkpf.
    CLEAR: v_zuonr,
           v_sgtxt,
           v_bschl,
           v_mwskz,
           ti_salida.

    CALL FUNCTION 'PROGRESS_INDICATOR'
      EXPORTING
        i_text               = 'Procesando documento  &1, sociedad &2'
        i_msgid              = '00'
        i_msgno              = '398'
        i_msgv1              = ti_bkpf-belnr
        i_msgv2              = ti_bkpf-bukrs
        i_processed          = sy-tabix
        i_total              = linea
        i_output_immediately = 'X'.

    ti_salida-street      = street.
    ti_salida-str_suppl1  = str_suppl1.
    ti_salida-str_suppl2  = v_suppl2.

    SELECT SINGLE zuonr sgtxt bschl
       INTO (v_zuonr,v_sgtxt,v_bschl)
       FROM bseg
       WHERE bukrs = ti_bkpf-bukrs
       AND   belnr = ti_bkpf-belnr
       AND   gjahr = ti_bkpf-gjahr
       AND   koart = 'K'
       AND   lifnr NE space.

    SELECT SINGLE mwskz
      INTO v_mwskz
      FROM bset
      WHERE bukrs = ti_bkpf-bukrs
      AND   belnr = ti_bkpf-belnr
      AND   gjahr = ti_bkpf-gjahr.

*** Inicio Genderson Gamboa cambio and por or
** Nro de Factura
*    IF ti_bkpf-blart EQ 'KR' AND ti_bkpf-blart EQ 'RE'.
    IF ti_bkpf-blart EQ 'KR' OR ti_bkpf-blart EQ 'RE' OR ti_bkpf-blart
    EQ 'KZ' .

      MOVE: v_zuonr TO ti_salida-zuonr.

    ENDIF.
*** Fin Genderson Gamboa

    CLEAR ti_salida-sgtxt.
    LOOP AT ti_bsak WHERE belnr EQ ti_bkpf-belnr.
      MOVE: ti_bsak-bukrs TO ti_salida-bukrs,
            ti_bsak-lifnr TO ti_salida-lifnr,
            ti_bsak-belnr TO ti_salida-belnr,
*            ti_bsak-zuonr TO ti_salida-zuonr,
*            ti_bsak-wrbtr TO ti_salida-wrbtr,
            ti_bsak-budat TO ti_salida-augdt.

      CLEAR v_qsatz.
      LOOP AT ti_with_item WHERE belnr EQ ti_bsak-belnr.

** Numero de comprobante
        CONCATENATE ti_bsak-budat(6) ti_with_item-ctnumber+2(8)
        INTO ti_salida-cont.
        MOVE: ti_with_item-wt_qsshb TO ti_salida-wrbtr3, "Impuesto IVA
              ti_with_item-wt_qbshb TO ti_salida-wrbtr4.

        MOVE ti_with_item-qsatz TO v_qsatz.

      ENDLOOP.

      LOOP AT ti_lfa1 WHERE lifnr EQ ti_bsak-lifnr.

** RIF Sujeto
        MOVE: ti_lfa1-stcd1 TO ti_salida-stcd_suj.
**

        LOOP AT ti_adrc WHERE addrnumber EQ ti_lfa1-adrnr.
          CONCATENATE  ti_adrc-name1
                       ti_adrc-name2
                       INTO ti_salida-name1 SEPARATED BY ''.

          CONCATENATE ti_adrc-street
                      ti_adrc-str_suppl1
                      ti_adrc-str_suppl2
                      ti_adrc-str_suppl3
                      INTO ti_salida-stras.
        ENDLOOP.



      ENDLOOP.

*    Cuando sean Notas de Credito o Debito busca la factura afectada
      IF ti_bsak-blart EQ 'Z6' OR ti_bsak-blart EQ 'KG' OR  " Credito
         ti_bsak-blart EQ 'KN'.                             " Debito
        IF ti_bsak-rebzg IS NOT INITIAL.
          PERFORM buscar_factura_afectada USING ti_bsak-bukrs
                                                ti_bsak-rebzg
                                                ti_bsak-rebzj
                                       CHANGING ti_salida-sgtxt.
        ENDIF.
      ENDIF.

    ENDLOOP.

*    ti_salida-wrbtr = ti_salida-wrbtr3 +
*                      ti_salida-wrbtr2. "Total compras

** Nro de Control
    IF ti_bkpf-blart EQ 'KR' OR ti_bkpf-blart EQ 'RE' OR ti_bkpf-blart
    EQ 'KZ'.
* JSOSA 22.11.2011
*      MOVE: ti_bkpf-xblnr TO ti_salida-xblnr,
*            ti_bkpf-bktxt TO ti_salida-bktxt.
      CONDENSE: ti_bkpf-xblnr, ti_bkpf-xref1_hd.
      CONCATENATE ti_bkpf-xblnr ti_bkpf-xref1_hd INTO ti_salida-xblnr.
      CONDENSE ti_salida-xblnr.
      MOVE ti_bkpf-bktxt TO ti_salida-bktxt.
* JSOSA 22.11.2011
    ENDIF.
**

** Nota de Credito
    IF ti_bkpf-blart EQ 'Z6' OR ti_bkpf-blart EQ 'KG'.
      CONDENSE: ti_bkpf-xblnr, ti_bkpf-xref1_hd.
      CONCATENATE ti_bkpf-xblnr ti_bkpf-xref1_hd
      INTO ti_salida-xblnrnc.
      CONDENSE ti_salida-xblnrnc.

      MOVE ti_bkpf-bktxt TO ti_salida-bktxt.
*            v_zuonr       TO ti_salida-sgtxt.

      CLEAR ti_salida-xblnr.
*      IF rb2 EQ 'X'.  " Cuando es el TXT se llena este campo
*        MOVE ti_salida-xblnrnc TO ti_salida-xblnr.
*      ENDIF.
* JSOSA 22.11.2011

    ENDIF.
** VOY AQUI REVISAR LIBRO COMPRAS
*    IF ti_bkpf-blart EQ 'KI'.
*      wa_alv-tciiva = monto1 + wa_bseg-dmbtr.
*    ENDIF.
**
** Nota de Debito
    IF ti_bkpf-blart EQ 'KN'.
      CONDENSE: ti_bkpf-xblnr, ti_bkpf-xref1_hd.
      CONCATENATE ti_bkpf-xblnr ti_bkpf-xref1_hd
      INTO ti_salida-xblnrnd.
      CONDENSE ti_salida-xblnrnd.

      MOVE ti_bkpf-bktxt TO ti_salida-bktxt.
*            v_zuonr       TO ti_salida-sgtxt.

      CLEAR ti_salida-xblnr.
*      IF rb2 EQ 'X'. " Cuando es el TXT se llena este campo
*        MOVE ti_salida-xblnrnd TO ti_salida-xblnr.
*      ENDIF.
* JSOSA 22.11.2011
    ENDIF.
**

*** Rif Agente
    ti_salida-stcd_agen = rif.


** Fecha de Factura
    IF  ti_bkpf-blart EQ 'RE' OR
        ti_bkpf-blart EQ 'KR' OR
        ti_bkpf-blart EQ 'KG' OR
        ti_bkpf-blart EQ 'KZ' OR
        ti_bkpf-blart EQ 'KN'.
      MOVE: ti_bkpf-bldat TO ti_salida-bldat.
    ENDIF.

    ti_salida-blart   = ti_bkpf-blart.

* JSOSA  27.05.2011    Inicio
    CASE ti_bkpf-blart.
      WHEN 'KR'.          "Facturas
        ti_salida-trans = '01'.
      WHEN 'RE'. "Facturas
        IF v_bschl EQ '31' . "Facturas
          ti_salida-trans = '01'.
        ELSEIF v_bschl EQ '21' . "Nota de debito
          ti_salida-trans = '02'.
        ENDIF.
      WHEN 'KN'.         "Nota de debito
        ti_salida-trans = '02'.
      WHEN 'KG' OR 'Z6'.         "Nota de crédito
        ti_salida-trans = '03'.
    ENDCASE.

    CLEAR t_salida_tmp. FREE t_salida_tmp.
    SORT ti_bset BY mwskz.
    LOOP AT ti_bset WHERE belnr EQ ti_bkpf-belnr.
      MOVE-CORRESPONDING ti_bset TO t_salida_tmp.
*  Excento V7
      IF ti_bset-mwskz NE 'V7' AND ti_bset-mwskz NE 'V8' AND
        t_salida_tmp-mwskz NE 'V0' AND t_salida_tmp-mwskz NE '41' AND
        t_salida_tmp-mwskz NE '51' AND ti_bset-mwskz NE space.
        t_salida_tmp-qsatz = ti_bset-kbetr / 10.
        t_salida_tmp-wrbtr2 = t_salida_tmp-wrbtr2 + ti_bset-hwbas.
        t_salida_tmp-wrbtr1 = t_salida_tmp-wrbtr1 + ti_bset-hwbas.
      ELSE.
        CLEAR t_salida_tmp-qsatz.
        IF ti_bset-mwskz EQ space.
          t_salida_tmp-mwskz = 'V7'.
        ELSE.
          t_salida_tmp-mwskz = ti_bset-mwskz.
        ENDIF.
        t_salida_tmp-wrbtr1 = t_salida_tmp-wrbtr1 + ti_bset-hwbas.
      ENDIF.

*  Convierte todos los montos a su valor absoluto para las
*  operaciones algebraicas en el smartforms
      t_salida_tmp-wrbtr1  =  abs( t_salida_tmp-wrbtr1 ).
      t_salida_tmp-wrbtr2  =  abs( t_salida_tmp-wrbtr2 ).

*  Se llena en esta tabla temporal la información de todas las
*  lineas que van al comprobante con su respectivo ind.Ret.
      COLLECT t_salida_tmp.
      CLEAR   t_salida_tmp.
    ENDLOOP.

* Guarda el valor real de acreedor en el campo LIFNR1 y arma una
* clave unica para grupo de registros (posiciones del no. de doc.)
* para indicarle al smartforms que va a saltar de pagina cada vez
* que campo de valor el campo LIFNR
    MOVE ti_salida-lifnr TO ti_salida-lifnr1.
    MOVE syst-tabix TO contador.
    CONCATENATE 'A-' contador INTO ti_salida-lifnr.
********

* Al finalizar de recopilar los datos va agregando las lineas
* necesarias a la tabla final de datos  TI_SALIDA
    AT END OF belnr.

* Ordena la tabla en forma descendente para que tome el
* ind. de Ret. vigente
      SORT t_salida_tmp BY mwskz DESCENDING.
* Busca si hay posiciones excentas de IVA para agregarlas
* a la primera fila de la tabla TI_SALIDA
      CLEAR w_salida_tmp.
      READ TABLE t_salida_tmp INTO w_salida_tmp WITH KEY mwskz = 'V0'.

* Recorre la tabla donde estan todos los valores con sus retenciones
* para ir agregando lineas según la retención, la información
* excenta se agrega en la primera linea
*      break abap1.
*      LOOP AT t_salida_tmp WHERE mwskz NE 'V7'.
      LOOP AT t_salida_tmp.
        IF  t_salida_tmp-mwskz NE 'V7' AND t_salida_tmp-mwskz NE 'V8'
        AND
            t_salida_tmp-mwskz NE 'V0' AND t_salida_tmp-mwskz NE '41'
            AND
             t_salida_tmp-mwskz NE '51' AND t_salida_tmp-mwskz NE 'V8'
        AND t_salida_tmp-mwskz NE space.
          IF syst-tabix EQ 1.     " Se agrega la info. de excentos

            MOVE: "w_salida_tmp-wrbtr1  TO  ti_salida-wrbtr1,
                  t_salida_tmp-wrbtr2  TO  ti_salida-wrbtr2,
                  t_salida_tmp-qsatz   TO ti_salida-qsatz.

            ti_salida-wrbtr3 = t_salida_tmp-wrbtr2 *
                               t_salida_tmp-qsatz / 100.

            IF v_qsatz IS NOT INITIAL.
              ti_salida-wrbtr4 = ti_salida-wrbtr3 * v_qsatz / 100.
            ENDIF.

            ti_salida-wrbtr = ti_salida-wrbtr + ( ti_salida-wrbtr1 +
                                                  ti_salida-wrbtr2 +
                                                  ti_salida-wrbtr3 ).
          ELSE.
            MOVE: t_salida_tmp-wrbtr2  TO  ti_salida-wrbtr2,
                  t_salida_tmp-qsatz   TO  ti_salida-qsatz.

            ti_salida-wrbtr3 = t_salida_tmp-wrbtr2 *
                               t_salida_tmp-qsatz / 100.

            IF ti_salida-qsatz EQ 0.
              ti_salida-wrbtr1 = t_salida_tmp-wrbtr1.
            ENDIF.

            IF v_qsatz IS NOT INITIAL.
              ti_salida-wrbtr4 = ti_salida-wrbtr3 * v_qsatz / 100.
            ENDIF.

*** Inicio Genderson Gamboa Correccion del calculo total wrbtr
            ti_salida-wrbtr = ti_salida-wrbtr + ( ti_salida-wrbtr1 +
                                                  ti_salida-wrbtr2 +
                                                  ti_salida-wrbtr3 ).
*            IF ti_bsak-pswbt EQ space.
*              ti_salida-wrbtr = ti_salida-wrbtr + ( ti_salida-wrbtr1 +
*                                                    ti_salida-wrbtr2 +
*                                                    ti_salida-wrbtr3 ).
*            ELSEIF ti_bsak-pswbt NE space.
*              ti_salida-wrbtr = ti_bsak-pswbt.
*            ENDIF.
*** Fin Genderson Gamboa
          ENDIF.

*  Agrega la linea a la tabla de salida y
*  luego borra los campos de montos
*  para las proximas lineas si hubiera
          APPEND ti_salida.
          CLEAR: ti_salida-qsatz,
                 ti_salida-wrbtr,
                 ti_salida-wrbtr1,
                 ti_salida-wrbtr2,
                 ti_salida-wrbtr3,
                 ti_salida-wrbtr4.
*        ELSEIF t_salida_tmp-mwskz EQ 'V7'.
        ELSE.   " Son  exentos
          ti_salida-wrbtr1    =  t_salida_tmp-wrbtr1.
          excento_iva-belnr   =  t_salida_tmp-belnr.
          excento_iva-wrbtr1  =  t_salida_tmp-wrbtr1.
          APPEND excento_iva.
          CLEAR excento_iva.
        ENDIF.
      ENDLOOP.
      CLEAR  ti_salida.
    ENDAT.
* JSOSA  27.05.2011    Fin

  ENDLOOP.

  DELETE ti_salida WHERE NOT lifnr1 IN p_lifnr.
*  DELETE ti_salida WHERE wrbtr4 EQ 0.
  SORT ti_salida BY lifnr1 ASCENDING bldat.

* Ciclo para numerar las lineas u operaciones por comprobante
* y cambiar el signo a las notas de crédito
  CLEAR zoper.
  LOOP AT ti_salida.

** Nro de Operacion
    zoper = zoper + 1.
    MOVE: zoper TO ti_salida-oper.

    IF ti_salida-blart EQ 'KG' OR      " Notas de crédito
       ti_salida-blart EQ 'Z6'.
      MULTIPLY: ti_salida-wrbtr  BY -1,
                ti_salida-wrbtr1 BY -1,
                ti_salida-wrbtr2 BY -1,
                ti_salida-wrbtr3 BY -1,
                ti_salida-wrbtr4 BY -1.
    ENDIF.

    MODIFY ti_salida.
    MOVE: ti_salida-bukrs  TO ti_cont-bukrs,
          ti_salida-cont   TO ti_cont-cont,
          ti_salida-lifnr  TO ti_cont-lifnr,
          ti_salida-lifnr1 TO ti_cont-lifnr1.
*    COLLECT ti_cont.    " JSOSA  27.05.2011
*    APPEND ti_cont.

    AT END OF lifnr.
      CLEAR zoper.
      APPEND ti_cont.
    ENDAT.


  ENDLOOP.

  SORT ti_salida BY cont.
  DELETE ti_salida WHERE wrbtr3 EQ 0 and wrbtr4 EQ 0.

  SORT ti_cont BY cont.

*BOM  id: oquintero-001 {
  DATA: wa_bset LIKE ti_bset2.

*  SELECT * FROM bset INTO TABLE ti_bset2
*    FOR ALL ENTRIES IN ti_bkpf
*    WHERE bukrs EQ p_bukrs
*    AND   belnr EQ ti_bkpf-belnr
*    AND   gjahr EQ ti_bkpf-gjahr
*    AND   ( ktosl EQ 'LUX' OR ktosl EQ 'VST' )
*    AND   ( mwskz EQ '35' OR mwskz EQ '45' ) .

  SELECT * FROM bset INTO TABLE ti_bset2
  FOR ALL ENTRIES IN ti_bkpf
  WHERE bukrs EQ p_bukrs
  AND   belnr EQ ti_bkpf-belnr
  AND   gjahr EQ ti_bkpf-gjahr
  AND  ( mwskz EQ '35' OR mwskz EQ '45' ) .

  LOOP AT ti_salida INTO wa_salida.
    LOOP AT ti_bset2 WHERE belnr EQ wa_salida-belnr.
      MOVE-CORRESPONDING wa_salida TO wa_total.
      COLLECT wa_total INTO ti_total.
      CLEAR: wa_salida.
    ENDLOOP.
  ENDLOOP.

  DELETE ti_total WHERE belnr EQ ''.

  CLEAR: wa_total, wa_salida.

  LOOP AT ti_total INTO wa_total.
    LOOP AT ti_salida INTO wa_salida
                      WHERE belnr EQ wa_total-belnr AND
                      cont EQ wa_total-cont.
      MOVE-CORRESPONDING wa_total TO wa_salida.
*      MOVE:
*      wa_total-belnr TO wa_salida-belnr,
*      wa_total-wrbtr TO wa_salida-wrbtr,
*      wa_total-wrbtr1 TO wa_salida-wrbtr1,
*      wa_total-wrbtr2 TO wa_salida-wrbtr2,
*      wa_total-wrbtr3 TO wa_salida-wrbtr3,
*      wa_total-wrbtr4 TO wa_salida-wrbtr4,
*      wa_total-qsatz  TO wa_salida-qsatz.

      MODIFY ti_salida FROM wa_salida.
    ENDLOOP.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM ti_salida COMPARING belnr.

* Monto Exento
  LOOP AT ti_salida.
    ld_tabix = sy-tabix.
    READ TABLE excento_iva WITH KEY belnr = ti_salida-belnr.
    ti_salida-wrbtr1 = excento_iva-wrbtr1.
    ti_salida-wrbtr = ti_salida-wrbtr1 +
                      ti_salida-wrbtr2 + ti_salida-wrbtr3.
    MODIFY ti_salida FROM ti_salida INDEX ld_tabix.
  ENDLOOP.


ENDFORM.                    "LLENAR_TABLA

*&---------------------------------------------------------------------*
*&      Form  FORMULARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM formulario .
  DATA: tdsfname TYPE tdsfname VALUE 'ZLAT_FI_SF_0004',
      fm_name TYPE rs38l_fnam.
  DATA: lv_flag(1) TYPE c.


  IF ti_bset2[] IS NOT INITIAL.
    lv_flag = 'X'.

  ELSE.
    lv_flag = ''.
  ENDIF.

  IF ti_salida[] IS NOT INITIAL.

    SORT ti_salida BY cont.
    SORT ti_cont  BY cont.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = tdsfname
      IMPORTING
        fm_name            = fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.


*    IF lv_flag EQ ''.
    break:latssilva.
    CALL FUNCTION fm_name
      EXPORTING
        p_bukrs   = p_bukrs
      TABLES
        ti_salida = ti_salida
        ti_cont   = ti_cont.

*    ELSE. "agrupo por indicadores

*      CALL FUNCTION fm_name
*      EXPORTING
*        p_bukrs   = p_bukrsyu
*        p_flag    = lv_flag
*        TABLES
*          ti_salida = ti_salida
*          ti_cont   = ti_cont
*          ti_total  = ti_total.
*    ENDIF.


  ELSE.

    MESSAGE i208(00) WITH 'No existen Datos para esta Selección'.

    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM.                    " FORMULARIO
*&---------------------------------------------------------------------*
*&      Form  DESCARGA_TXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM descarga_txt .

  MOVE p_file TO v_file.


  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE          =
      filename              = v_file
*     FILETYPE              = 'TXT'
*     APPEND                = ' '
      write_field_separator = 'X'
*     HEADER                = '00'
      trunc_trailing_blanks = 'X'
*     WRITE_LF              = 'X'
*     COL_SELECT            = ' '
*     COL_SELECT_MASK       = ' '
*     DAT_MODE              = ' '
*     CONFIRM_OVERWRITE     = ' '
*     NO_AUTH_CHECK         = ' '
*     CODEPAGE              = ' '
*     IGNORE_CERR           = ABAP_TRUE
*     REPLACEMENT           = '#'
*     WRITE_BOM             = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT          = ' '
*     WK1_N_SIZE            = ' '
*     WK1_T_FORMAT          = ' '
*     WK1_T_SIZE            = ' '
*     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*     SHOW_TRANSFER_STATUS  = ABAP_TRUE
* IMPORTING
*     FILELENGTH            =
    TABLES
      data_tab              = it_descarga
*     FIELDNAMES            =
* EXCEPTIONS
*     FILE_WRITE_ERROR      = 1
*     NO_BATCH              = 2
*     GUI_REFUSE_FILETRANSFER         = 3
*     INVALID_TYPE          = 4
*     NO_AUTHORITY          = 5
*     UNKNOWN_ERROR         = 6
*     HEADER_NOT_ALLOWED    = 7
*     SEPARATOR_NOT_ALLOWED = 8
*     FILESIZE_NOT_ALLOWED  = 9
*     HEADER_TOO_LONG       = 10
*     DP_ERROR_CREATE       = 11
*     DP_ERROR_SEND         = 12
*     DP_ERROR_WRITE        = 13
*     UNKNOWN_DP_ERROR      = 14
*     ACCESS_DENIED         = 15
*     DP_OUT_OF_MEMORY      = 16
*     DISK_FULL             = 17
*     DP_TIMEOUT            = 18
*     FILE_NOT_FOUND        = 19
*     DATAPROVIDER_EXCEPTION          = 20
*     CONTROL_FLUSH_ERROR   = 21
*     OTHERS                = 22
    .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.



ENDFORM.                    " DESCARGA_TXT

*&---------------------------------------------------------------------*
*&      Form  BLOQUEAR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bloquear_campos .

  LOOP AT SCREEN.

*    IF rb1 EQ 'X' OR rb3 EQ 'X' OR rb4 EQ 'X'.
*      IF screen-name = 'P_FILE'.
*        screen-input = 0.
*      ENDIF.

*    ELSEIF rb2 EQ 'X'.
*      IF screen-name = 'P_FILE'.
*        screen-input = 1.
*      ENDIF.

*    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " BLOQUEAR_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  ABRIR_ARCHIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM abrir_archivo .

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = 'Descargar en'
    CHANGING
      selected_folder = v_path.
  IF sy-subrc EQ 0.

    CONCATENATE v_path '\Retencion_IVA.txt' INTO p_file.

  ENDIF.

ENDFORM.                    " ABRIR_ARCHIVO
*&---------------------------------------------------------------------*
*&      Form  LLENAR_TABLA_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM llenar_tabla_desc .

  DATA: v_fecha(10) TYPE c.
  DATA: v_monto TYPE string,
        v_monto1 TYPE string,
        v_monto2 TYPE string,
        v_monto4 TYPE string.

  FIELD-SYMBOLS: <fs_bkpf> LIKE LINE OF ti_bkpf2.
  break abap1.

  ti_bkpf2[] = ti_bkpf[].

*  DELETE ti_bkpf2 WHERE stgrd IS NOT INITIAL.

  LOOP AT ti_bkpf2 ASSIGNING <fs_bkpf> WHERE stgrd IS NOT INITIAL.
    DELETE ti_salida WHERE belnr EQ <fs_bkpf>-belnr.
    EXIT.
  ENDLOOP.

  LOOP AT ti_salida.

    READ TABLE excento_iva WITH KEY belnr = ti_salida-belnr.
    IF sy-subrc = 0.
      ti_salida-wrbtr1 = excento_iva-wrbtr1.
    ENDIF.
    CLEAR: wa_descarga,
           v_fecha.

    CONCATENATE ti_salida-bldat(4) '-' ti_salida-bldat+4(2) '-'
                ti_salida-bldat+6(2) INTO v_fecha.

    "Tipo de documento
    IF ti_salida-blart EQ 'KR' OR ti_salida-blart EQ 'RE'.

      wa_descarga-tdoc = '01'.

    ELSEIF ti_salida-blart EQ 'KN'.

      wa_descarga-tdoc = '02'.

    ELSEIF ti_salida-blart EQ 'KG'.

      wa_descarga-tdoc = '03'.

    ENDIF.

    wa_descarga-oper = 'C'. "Acreedor

    REPLACE ALL OCCURRENCES OF '-' IN ti_salida-stcd_agen WITH space.
    REPLACE ALL OCCURRENCES OF '-' IN ti_salida-stcd_suj  WITH space.
    CONDENSE: ti_salida-stcd_agen, ti_salida-stcd_suj.

    MOVE: ti_salida-stcd_agen TO wa_descarga-paval,
                                         "Rif del agente
          v_fecha             TO wa_descarga-v_bldat,
                                         "Fecha de la factura
          ti_salida-stcd_suj  TO wa_descarga-stcd1,
                                         "Rif del proveedor
          ti_salida-xblnr     TO wa_descarga-xblnr,
                                         "Numero del documento
*          ti_salida-zuonr     TO wa_descarga-zuonr,
                                         "Numero de control
          ti_salida-qsatz     TO wa_descarga-qsatz,
                                         "Alicuota
          ti_salida-cont      TO wa_descarga-cont,
                                         "Nro de Comprobante
          ti_salida-sgtxt     TO wa_descarga-sgtxt,
                                        "Numero del documento afectado
          ti_salida-bktxt     TO wa_descarga-bktxt,
                                        "Numero del documento afectado
          ti_salida-wrbtr4    TO wa_descarga-wrbtr4,
                                         "IVA retenido
          ti_salida-wrbtr2    TO wa_descarga-wrbtr2,
                                          "Base imponible
          ti_salida-wrbtr1    TO wa_descarga-wrbtr1,
                                         "Monto exento del IVA
          ti_salida-wrbtr     TO wa_descarga-wrbtr,
                                         "Total compras
*          ti_salida-bldat(6)  TO wa_descarga-imp,
                                         "Periodo impositivo
          p_budat-low(6)      TO wa_descarga-imp,
                                         "Periodo impositivo
          '0'                 TO wa_descarga-cero.

    CONDENSE: ti_salida-stcd_agen,
              wa_descarga-paval,     "Rif del agente
              wa_descarga-v_bldat,   "Fecha de la factura
              wa_descarga-stcd1,     "Rif del proveedor
              wa_descarga-xblnr,     "Numero del documento
*              wa_descarga-zuonr,     "Numero de control
              wa_descarga-cont,      "Nro de Comprobante
              wa_descarga-sgtxt.     "Numero del documento afectado


*BOM  id: oquintero-001 { Ajustes para las NC con montos negativos
    CLEAR: v_monto, v_monto1, v_monto2, v_monto4.
    v_monto = wa_descarga-wrbtr.
    v_monto1 = wa_descarga-wrbtr1.
    v_monto2 = wa_descarga-wrbtr2.
    v_monto4 = wa_descarga-wrbtr4.

    REPLACE ALL OCCURRENCES OF '-' IN v_monto WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN v_monto1 WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN v_monto2 WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN v_monto4 WITH ''.

    MOVE:
    v_monto  TO wa_descarga-wrbtr,
    v_monto1 TO wa_descarga-wrbtr1,
    v_monto2 TO wa_descarga-wrbtr2,
    v_monto4 TO wa_descarga-wrbtr4.

* } EOM oquintero-001

    IF wa_descarga-paval IS INITIAL.
      MOVE '0' TO wa_descarga-paval.
    ENDIF.
    IF wa_descarga-imp IS INITIAL.
      MOVE '0' TO wa_descarga-imp.
    ENDIF.
    IF wa_descarga-v_bldat IS INITIAL.
      MOVE '0' TO wa_descarga-v_bldat.
    ENDIF.
    IF wa_descarga-oper IS INITIAL.
      MOVE '0' TO wa_descarga-oper.
    ENDIF.
    IF wa_descarga-tdoc IS INITIAL.
      MOVE '0' TO wa_descarga-tdoc.
    ENDIF.
    IF wa_descarga-stcd1 IS INITIAL.
      MOVE '0' TO wa_descarga-stcd1.
    ENDIF.
*    IF wa_descarga-xblnr IS INITIAL.
*      MOVE '0' TO wa_descarga-xblnr.
*    ENDIF.
    IF wa_descarga-bktxt IS INITIAL.
      MOVE '0' TO wa_descarga-bktxt.
    ENDIF.
    IF wa_descarga-wrbtr IS INITIAL.
      MOVE '0' TO wa_descarga-wrbtr.
    ENDIF.
    IF wa_descarga-wrbtr2 IS INITIAL.
      MOVE '0' TO wa_descarga-wrbtr2.
    ENDIF.
    IF wa_descarga-wrbtr4 IS INITIAL.
      MOVE '0' TO wa_descarga-wrbtr4.
    ENDIF.
    IF wa_descarga-sgtxt IS INITIAL.
      MOVE '0' TO wa_descarga-sgtxt.
    ENDIF.
    IF wa_descarga-cont IS INITIAL.
      MOVE '0' TO wa_descarga-cont.
    ENDIF.
    IF wa_descarga-wrbtr1 IS INITIAL.
      MOVE '0' TO wa_descarga-wrbtr1.
    ENDIF.
    IF wa_descarga-qsatz IS INITIAL.
      MOVE '0' TO wa_descarga-qsatz.
    ENDIF.
    IF wa_descarga-cero IS INITIAL.
      MOVE '0' TO wa_descarga-cero.
    ENDIF.

    IF wa_descarga-tdoc = '02'. "ND

      MOVE ti_salida-xblnrnd TO wa_descarga-xblnr.


    ELSEIF wa_descarga-tdoc = '03'. "NC

      MOVE ti_salida-xblnrnc TO wa_descarga-xblnr.

    ENDIF.

    APPEND wa_descarga TO it_descarga.

  ENDLOOP.


ENDFORM.                    " LLENAR_TABLA_DESC

*&---------------------------------------------------------------------*
*&      Form  SELECCIONAR_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleccionar_documentos .

* Selecciona los documentos financieros los que
* anulan a las facturas por el área de MM
  LOOP AT rt_bkpf WHERE tcode EQ 'MR8M'.
    MOVE rt_bkpf TO t_bkpf_1.
    APPEND t_bkpf_1.
  ENDLOOP.

* Selecciona los documentos anulados
  LOOP AT rt_bkpf WHERE stblg NE space.
    MOVE rt_bkpf TO t_bkpf_1.
    APPEND t_bkpf_1.
  ENDLOOP.

ENDFORM.                    " SELECCIONAR_DOCUMENTOS

*&---------------------------------------------------------------------*
*&      Form  DEPURAR_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM depurar_documentos .

  DATA: t_bseg_1 LIKE bseg OCCURS 0 WITH HEADER LINE,
        t_bsak_1 LIKE bsak OCCURS 0 WITH HEADER LINE,
        v_tabix  LIKE syst-tabix,
        ld_tabix TYPE sy-tabix,
        indice   TYPE i.
  break: latssilva.
  IF t_bkpf_1[] IS INITIAL. EXIT. ENDIF.

* Busca los documentos de compensación
  SELECT *
  INTO TABLE t_bseg_1
  FROM bseg
  FOR ALL ENTRIES IN t_bkpf_1
  WHERE bukrs EQ t_bkpf_1-bukrs
    AND belnr EQ t_bkpf_1-belnr
    AND gjahr EQ t_bkpf_1-gjahr
    AND koart EQ 'K'
    AND augbl NE space.

  IF t_bseg_1[] IS INITIAL. EXIT. ENDIF.

* Busca los documentos compensados
  SELECT *
  INTO TABLE t_bsak_1
  FROM bsak
  FOR ALL ENTRIES IN t_bseg_1
  WHERE augbl EQ t_bseg_1-augbl
    AND augdt EQ t_bseg_1-augdt.

* Borra los documentos que fueron anulados y los
* documentos que anulan
  LOOP AT t_bsak_1.
    READ TABLE ti_salida WITH KEY bukrs = t_bsak_1-bukrs
                                  belnr = t_bsak_1-belnr.
    CHECK syst-subrc EQ 0.
    indice = sy-tabix.
*
    READ TABLE ti_bkpf WITH KEY bukrs = ti_salida-bukrs
                                belnr = ti_salida-belnr.
    IF ti_bkpf-tcode EQ 'MR8M' OR
       ti_bkpf-stblg NE space.
*    Busca el documento que anula para ubicar el documento
*    que anulo para sacarlo del listado tambien
      CLEAR t_bseg_1.
      READ TABLE t_bseg_1 WITH KEY bukrs = ti_salida-bukrs
                                   belnr = ti_salida-belnr.
      IF syst-subrc EQ 0.
*      Saca de la lista el documento que fue anulado
        READ TABLE ti_salida WITH KEY bukrs = t_bseg_1-bukrs
                                      belnr = t_bseg_1-rebzg.
        IF syst-subrc EQ 0.
          DELETE ti_salida INDEX syst-tabix.
        ENDIF.
      ENDIF.
*    Borra el documento de Anulador
      DELETE ti_salida INDEX indice.
    ENDIF.
    CLEAR indice.
*
  ENDLOOP.

* Ciclo para numerar las lineas u operaciones por comprobante
**BOM  id: oquintero-001 { comentado por error al enviar correo
  CLEAR: ti_cont. FREE ti_cont.
  CLEAR zoper.

  LOOP AT ti_salida.
    ld_tabix = sy-tabix.
*   monto exento
    READ TABLE excento_iva WITH KEY belnr = ti_salida-belnr.
    ti_cont-wrbtr1 = excento_iva-wrbtr1.

    MODIFY ti_salida FROM ti_salida INDEX ld_tabix.

** Nro de Operacion
    zoper = zoper + 1.
    MOVE: zoper TO ti_salida-oper.
    MODIFY ti_salida.
    MOVE :  ti_salida-bukrs  TO ti_cont-bukrs,
            ti_salida-cont   TO ti_cont-cont,
            ti_salida-lifnr  TO ti_cont-lifnr,
            ti_salida-lifnr1 TO ti_cont-lifnr1.
*    COLLECT ti_cont.    " JSOSA  27.05.2011
*    APPEND ti_cont.
*    CLEAR zoper.
    AT END OF lifnr.
      CLEAR zoper.
*      APPEND ti_cont.
      COLLECT ti_cont.
    ENDAT.

  ENDLOOP.
** } EOM oquintero-001

  SORT ti_salida BY cont.
  SORT ti_cont BY cont.
ENDFORM.                    " DEPURAR_DOCUMENTOS
*&---------------------------------------------------------------------
*&      Form  Header.
*&---------------------------------------------------------------------
*  Crea la Cabecera
*----------------------------------------------------------------------
FORM header.

  DATA: comment    TYPE slis_listheader,
        zcomment   TYPE slis_t_listheader,
        aux        TYPE string,
        ld_logo    TYPE bds_typeid,
        ld_fecha(10) TYPE c,
        ld_hora(10)  TYPE c.

  CONSTANTS: title TYPE slis_listheader-typ VALUE 'H',
             text  TYPE slis_listheader-typ VALUE 'S',
             text1 TYPE slis_listheader-typ VALUE 'S'.
  DATA: wa_t001 TYPE t001,
        wa_adrc TYPE adrc.

  SELECT SINGLE * FROM t001 INTO wa_t001
    WHERE bukrs = p_bukrs.
  SELECT SINGLE * FROM adrc INTO wa_adrc
    WHERE addrnumber = wa_t001-adrnr.
*title
  CLEAR comment.
  comment-typ  = title.
  comment-key  = ''.
*  CONCATENATE wa_t001-butxt 'C.A.' INTO comment-info
*  separated by space.
  MOVE wa_t001-butxt TO comment-info.
  APPEND comment TO zcomment.
  comment-typ  = text.
  comment-key  = ''.
  comment-info = rif.
  APPEND comment TO zcomment.

  comment-typ  = text.
  comment-key  = ''.
  comment-info = 'Listado de Comprobantes para Retención IVA'.
  APPEND comment TO zcomment.


  comment-typ = text.
  CASE p_budat-low+4(2).
    WHEN '01'.
      comment-info = 'Enero'.
    WHEN '02'.
      comment-info = 'Febrero'.
    WHEN '03'.
      comment-info = 'Marzo'.
    WHEN '04'.
      comment-info = 'Abril'.
    WHEN '05'.
      comment-info = 'Mayo'.
    WHEN '06'.
      comment-info = 'Junio'.
    WHEN '07'.
      comment-info = 'Julio'.
    WHEN '08'.
      comment-info = 'Agosto'.
    WHEN '09'.
      comment-info = 'Septiembre'.
    WHEN '10'.
      comment-info = 'Octubre'.
    WHEN '11'.
      comment-info = 'Noviembre'.
    WHEN '12'.
      comment-info = 'Diciembre'.
  ENDCASE.
  aux =   p_budat-low+0(4).
  CONCATENATE 'Correspondiente al Mes' comment-info aux
  INTO comment-info SEPARATED BY space.
  APPEND comment TO zcomment.

  CLEAR comment.
  comment-typ  = text.
  comment-key  = ''.
  CONCATENATE sy-datum+6(2) '/' sy-datum+4(2) '/' sy-datum+0(4)
  INTO ld_fecha.
  CONCATENATE sy-uzeit+0(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2)
  INTO ld_hora.
  CONCATENATE 'Generado el' ld_fecha 'a las' ld_hora
  INTO comment-info SEPARATED BY space.
  APPEND comment TO zcomment.

  PERFORM nombre_logo USING p_bukrs
                   CHANGING ld_logo.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
     i_logo             = ld_logo
      it_list_commentary = zcomment.

ENDFORM.                     "update_data
*&---------------------------------------------------------------------
*&      Form  create_catalog
*&---------------------------------------------------------------------
*  Crea el Catalogo
*----------------------------------------------------------------------
FORM create_catalog.
  REFRESH fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = zrepid
      i_internal_tabname = 'TI_SALIDA_ALV'
      i_inclname         = zrepid
    CHANGING
      ct_fieldcat        = lt_fieldcat.

  LOOP AT lt_fieldcat INTO w_fieldcat.

    CASE w_fieldcat-fieldname.
      WHEN 'BUKRS'.
        w_fieldcat-outputlen = '8'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'BELNR'.
        w_fieldcat-hotspot = 'X'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'GJAHR'.
        w_fieldcat-key     = ' '.
        w_fieldcat-outputlen = '8'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'CONT'.
        w_fieldcat-seltext_m = 'Nº Conprobante'.
        w_fieldcat-ddictxt   = 'M'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'NAME1'.
        w_fieldcat-seltext_m = 'Nombre'.
        w_fieldcat-ddictxt   = 'M'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'XBLNR'.
        w_fieldcat-seltext_m = 'Referencia'.
        w_fieldcat-ddictxt   = 'M'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'XBLNRND'.
        w_fieldcat-seltext_m = 'Nota de Débito'.
        w_fieldcat-ddictxt   = 'M'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'XBLNRNC'.
        w_fieldcat-seltext_m = 'Nota de Crédito'.
        w_fieldcat-ddictxt   = 'M'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'TRANS'.
        w_fieldcat-seltext_m = 'Tp.Trans'.
        w_fieldcat-ddictxt   = 'M'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'WRBTR'.
        w_fieldcat-seltext_m = 'T.Compras IVA Incl.'.
        w_fieldcat-ddictxt   = 'M'.
        w_fieldcat-do_sum   = 'X'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'WRBTR1'.
        w_fieldcat-seltext_m = 'Compras S/Créd.Fiscal'.
        w_fieldcat-ddictxt   = 'M'.
        w_fieldcat-do_sum   = 'X'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'WRBTR2'.
        w_fieldcat-seltext_m = 'Base Imponible'.
        w_fieldcat-ddictxt   = 'M'.
        w_fieldcat-do_sum   = 'X'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'WRBTR3'.
        w_fieldcat-seltext_m = 'Imp. IVA'.
        w_fieldcat-ddictxt   = 'M'.
        w_fieldcat-do_sum   = 'X'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'WRBTR4'.
        w_fieldcat-seltext_m = 'IVA Retenido'.
        w_fieldcat-ddictxt   = 'M'.
        w_fieldcat-do_sum   = 'X'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
      WHEN 'ICONO'.
        w_fieldcat-seltext_m = 'Status'.
        w_fieldcat-ddictxt   = 'M'.
        w_fieldcat-icon      = 'X'.
        w_fieldcat-outputlen = '8'.
        MODIFY lt_fieldcat FROM w_fieldcat INDEX syst-tabix.
    ENDCASE.

  ENDLOOP.

*  m_fieldcat ''  'OPER'      ' '  ' ' '1' ' ' 'Oper. Nº' '8'.
*  m_fieldcat ''  'BELNR'     ' '  ' ' ' ' 'X' 'Nº Documento' '12' .
*  m_fieldcat ''  'FFACTURA'  ' '  ' ' '2' ' ' 'Fecha Factura' '13'.
*  m_fieldcat ''  'RIF'       ' '  ' ' '3' ' ' 'RIF' '12'.

ENDFORM.                     "create_catalog

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_CORREOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enviar_correos .

  DATA: tdsfname TYPE tdsfname VALUE 'ZLAT_FI_SF_0004',
        fm_name  TYPE rs38l_fnam.

  DATA: ti_salida_mail LIKE ti_salida OCCURS 0 WITH HEADER LINE,
*        ti_cont_mail   LIKE ti_cont   OCCURS 0 WITH HEADER LINE.
        ti_cont_mail   TYPE TABLE OF zretencion_iva.

  DATA: wa_cont_mail TYPE zretencion_iva,
        wa_salida_mail LIKE LINE OF ti_salida_mail.

* Internal Table declarations
  DATA: i_otf_final TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_otf TYPE TABLE OF solisti1,
        i_tline TYPE TABLE OF tline WITH HEADER LINE,
        i_docs TYPE TABLE OF docs WITH HEADER LINE,
        i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record LIKE solisti1 OCCURS 0 WITH HEADER LINE,

** Objects to send mail.
        i_objpack LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist LIKE somlreci1 OCCURS 0 WITH HEADER LINE,

* Work Area declarations
        wa_objhead TYPE soli_tab,
        w_ctrlop TYPE ssfctrlop,
        w_compop TYPE ssfcompop,
        w_return TYPE ssfcrescl,
        wa_doc_chng TYPE sodocchgi1,
        w_data TYPE sodocchgi1,
        wa_buffer TYPE string,"To convert from 132 to 255

* Variables declarations
        v_form_name TYPE rs38l_fnam,
        v_len_in LIKE sood-objlen,
        v_len_out LIKE sood-objlen,
        v_len_outn TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i,

* Import of the bapi
        zsend_to_all LIKE  sonv-flag,
        znew_order   LIKE  sofolenti1-object_id.

  DATA: asunto_email(255) TYPE c.
  DATA: remitente LIKE soextreci1-receiver,
        remitente_em  LIKE adr6-smtp_addr.
  DATA: destinatario  LIKE adr6-smtp_addr,
        v_commit_work LIKE sonv-flag,
        iv_druvo      TYPE t166k-druvo VALUE '1',
        ent_retco     LIKE syst-subrc,
        ent_screen    TYPE char4.

  DATA: lt_destinatario TYPE TABLE OF adr6.

  DATA: i_texttable LIKE solisti1 OCCURS 0 WITH HEADER LINE.

  DATA: intab      TYPE soli_tab,
        wa_intab   TYPE LINE OF soli_tab,
        t_objhex   TYPE solix_tab,
        w_objhex   LIKE LINE OF t_objhex,
        wa_objbin  TYPE LINE OF solix_tab,
        objpack    TYPE TABLE OF sopcklsti1 WITH HEADER LINE.

******* Comienza el proceso de envio de correos
  CLEAR: ti_cont.

  ti_cont[] = ti_salida[].
*break latecolez.
  IF ti_salida[] IS NOT INITIAL.

    SORT ti_salida BY cont.
    SORT ti_cont  BY cont.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = tdsfname
      IMPORTING
        fm_name            = fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

*    LOOP AT ti_cont INTO ti_cont_mail.
*      APPEND ti_cont_mail.
    LOOP AT ti_cont INTO wa_cont_mail.
      CLEAR:    ti_salida_mail, ti_cont_mail.
      REFRESH:  ti_salida_mail, ti_cont_mail.
      APPEND wa_cont_mail TO ti_cont_mail.
      CLEAR wa_salida.
*      READ TABLE ti_salida INTO ti_salida_mail
*                           WITH KEY cont = ti_cont-cont.
      READ TABLE ti_salida INTO wa_salida
                           WITH KEY cont = wa_cont_mail-cont
                                    oper = wa_cont_mail-oper.
*      APPEND ti_salida_mail.
      MOVE wa_salida TO wa_salida_mail.
      APPEND wa_salida_mail TO ti_salida_mail.

**---- Parametros de control
      w_ctrlop-getotf       = 'X'.
      w_ctrlop-no_dialog    = 'X'.
      w_compop-tdnoprev     = 'X'.
      w_compop-tddest       = 'LOCL'.
      w_compop-tdarmod      = '1'.
      w_compop-tdcopies     = '1'.
      w_compop-tdprinter    = 'SAPWIN'.
      w_compop-tdlifetime   = '8'.

*>>>>> Change of Parameters <<<<<<<<<<<<<<<<<<<<<<<
      CALL FUNCTION fm_name
        EXPORTING
          control_parameters = w_ctrlop
          output_options     = w_compop
*         user_settings      = 'X'
*         is_nast            = l_nast
*         iv_from_mem        = l_from_memory
          iv_druvo           = iv_druvo
*         iv_xfz             = iv_xfz
          mail               = 'X'
          p_bukrs            = p_bukrs
        IMPORTING
          job_output_info    = w_return
        TABLES
          ti_salida          = ti_salida_mail
          ti_cont            = ti_cont_mail
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.

      IF sy-subrc <> 0.
        ent_retco = sy-subrc.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*******************************************************************
***** JSOSA  20.10.2011 Begin
*********** Inicio del proceso de convertir smartforms a PDF
*********** y enviar por correo
*******************************************************************

      ent_retco = 0.  " Todo OK
      iv_druvo  = 1.  " Confirmación del Pedido

      i_otf_final[] = w_return-otfdata[].

      CLEAR intab.
      FREE: intab, t_objhex.

*En la tabla otfdata nos queda guardado el formulario en formtao OTF
*Ahora sólo resta convertir el OTF en PDF
      LOOP AT i_otf_final.
        wa_intab = i_otf_final.
        APPEND wa_intab TO intab.
        CLEAR wa_intab.
      ENDLOOP.

*Convertir el OTF en PDF
      CALL FUNCTION 'SX_OBJECT_CONVERT_OTF_PDF'
        EXPORTING
          format_src      = 'OTF'
          format_dst      = 'PDF'
*         ADDR_TYPE       =
          devtype         = 'LP01'
*         FUNCPARA        =
        CHANGING
          transfer_bin    = objpack-transf_bin
          content_txt     = intab
          content_bin     = t_objhex
          objhead         = intab
          len             = objpack-doc_size
        EXCEPTIONS
          err_conv_failed = 1
          OTHERS          = 2.

      IF sy-subrc <> 0.
        ent_retco = sy-subrc.
        IF syst-msgid IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ELSE.
        CLEAR: objpack-transf_bin, objpack-doc_size.

*        CALL FUNCTION 'GUI_DOWNLOAD'
*          EXPORTING
*            filetype = 'BIN'
*            filename = 'C:\temp\comprobante_iva.pdf'
*          TABLES
*            data_tab = t_objhex.
      ENDIF.

*-------------------------------------------------------------
* Rellena el cuerpo de e-mail a enviar
*-------------------------------------------------------------
      DATA: gv_texto(250) TYPE c,
      gv_texto1(100) TYPE c,
      gv_texto2(100) TYPE c,
      gv_texto3(50) TYPE c,
      gv_texto4(350) TYPE c,
      gv_texto5(100) TYPE c,
      gv_var(250) TYPE c VALUE '                                ',
      gv_var1(25) TYPE c VALUE 'Laboratorio Behrens, C.A.',
      gv_var2(12) TYPE c,
      gv_var3(25) TYPE c VALUE 'Estimado proveedor:',
      gv_mes(6) TYPE c,
      gv_name TYPE string,
      gv_fecha(10) TYPE c.

      gv_mes = ti_salida-bldat+4(2).

      CASE gv_mes.
        WHEN '01'.
          gv_name = 'Enero'.
        WHEN  '02'.
          gv_name = 'Febrero'.
        WHEN '03'.
          gv_name = 'Marzo'.
        WHEN '04'.
          gv_name = 'Abril'.
        WHEN '05'.
          gv_name = 'Mayo'.
        WHEN '06'.
          gv_name = 'Junio'.
        WHEN '07'.
          gv_name = 'Julio'.
        WHEN '08'.
          gv_name = 'Agosto'.
        WHEN '09'.
          gv_name = 'Septiembre'.
        WHEN '10'.
          gv_name = 'Octubre'.
        WHEN '11'.
          gv_name = 'Noviembre'.
        WHEN '12'.
          gv_name = 'Diciembre'.
        WHEN OTHERS.
      ENDCASE.

      CLEAR i_texttable. FREE i_texttable.
      i_texttable-line =  gv_var1.
      APPEND i_texttable.
      i_texttable-line =  gv_var.
      APPEND i_texttable.

      gv_var2 = wa_salida-stcd_agen.
      i_texttable-line =  gv_var2.
      APPEND i_texttable.
      i_texttable-line =  gv_var.
      APPEND i_texttable.

      i_texttable-line =  gv_var3.
      APPEND i_texttable.
      i_texttable-line =  gv_var.
      APPEND i_texttable.
      i_texttable-line =  gv_var.
      APPEND i_texttable.
      i_texttable-line =  gv_var.
      APPEND i_texttable.

*"Conversion Exit for Domain GBDAT: YYYYMMDD -> DD/MM/YYYY
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
        EXPORTING
         date_internal                  = wa_salida-augdt
        IMPORTING
         date_external                  = gv_fecha.

      REPLACE ALL OCCURRENCES OF '.' IN gv_fecha WITH ''.
      WRITE gv_fecha TO gv_fecha USING EDIT MASK '__/__/____'.

      CONCATENATE 'Anexo al presente estamos enviando de forma digital '
      'comprobante de retención de IVA Nº' wa_salida-cont
      ' de fecha ' gv_fecha', ' 'de la Empresa: ' wa_salida-name1
      '.' INTO gv_texto RESPECTING BLANKS.
      i_texttable-line = gv_texto.
      APPEND i_texttable.

     CONCATENATE 'Correspondiente al pago de la facturación del mes de '
     gv_name '.' ' Los cuales autorizamos a que sean procesados'
     ' de forma digital.'
     INTO gv_texto1 RESPECTING BLANKS.
      i_texttable-line = gv_texto1.
      APPEND i_texttable.
      i_texttable-line =  gv_var.
      APPEND i_texttable.
      i_texttable-line =  gv_var.
      APPEND i_texttable.

      gv_texto2 = 'Sin más a que hacer referencia, se despide.'.
      i_texttable-line = gv_texto2.
      APPEND i_texttable.
      i_texttable-line =  gv_var.
      APPEND i_texttable.
      i_texttable-line =  gv_var.
      APPEND i_texttable.

      gv_texto3 = 'Atentamente,'.
      i_texttable-line = gv_texto3.
      APPEND i_texttable.
      i_texttable-line =  gv_var.
      APPEND i_texttable.

      CONCATENATE
  'Nota: Si usted requiere los documentos originales con sello húmedo, '
  'por favor tramitar la solicitud por esta vía, y pasarlos a retirar '
  'la semana siguiente después de haber recibido esta notificación.'
  INTO gv_texto4 RESPECTING BLANKS.
      i_texttable-line = gv_texto4.
      APPEND i_texttable.

      CONCATENATE
      'En horario comprendido de lunes a Jueves de 09:00 AM A 11:00 am.'
      ' – 02:00 a 04:pm. '
      INTO gv_texto5 RESPECTING BLANKS.
      i_texttable-line = gv_texto5.
      APPEND i_texttable.

      MOVE 'Comprobante de Retención de IVA' TO asunto_email.

*-------------------------------------------------------------
* Bajar a disco el smartforms convertido a PDF
*-------------------------------------------------------------
*  PERFORM grabar_pdf_endisco.

*-------------------------------------------------------------
* Buscar Destinatario y Remitente
*-------------------------------------------------------------
*
      PERFORM buscar_remitente CHANGING remitente_em.
      IF remitente_em IS INITIAL. CONTINUE. ENDIF.
*
*      PERFORM buscar_destinatario USING ti_salida-lifnr1
*                               CHANGING destinatario.


*BOM  id: oquintero-001 { Consulto destinatario
      DATA: v_addrnumber LIKE adrc-addrnumber,
            v_lifnr      LIKE lfa1-lifnr.
      DATA: wa_destinatario TYPE adr6.

      p_lifnr = ti_salida-lifnr1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_lifnr
      IMPORTING
        output = v_lifnr.

      SELECT SINGLE adrnr INTO v_addrnumber
      FROM lfa1
      WHERE lifnr EQ v_lifnr.

      SELECT * FROM adr6
        INTO TABLE lt_destinatario
        WHERE addrnumber EQ v_addrnumber
        AND smtp_addr NE space.

* } EOM oquintero-001

*      IF destinatario IS INITIAL. CONTINUE. ENDIF.
      IF lt_destinatario[] IS INITIAL. CONTINUE. ENDIF.
*-------------------------------------------------------------
* Generar el EMAIL con el smartforms adjunto
*-------------------------------------------------------------
      REFRESH: i_reclist, i_objtxt, i_objpack.

      CLEAR: wa_objhead, wa_doc_chng.

* i_texttable es una tabla con un campo texto ( 255 caracteres )
* donde esta el texto que forma el cuerpo del email.
      i_objtxt[] = i_texttable[].

      DESCRIBE TABLE i_objtxt LINES v_lines_txt.
      READ TABLE i_objtxt INDEX v_lines_txt.
      wa_doc_chng-obj_name = asunto_email.
      wa_doc_chng-expiry_dat = sy-datum + 10.
      wa_doc_chng-obj_descr = asunto_email.  "--> ASUNTO DEL EMAIL
      wa_doc_chng-sensitivty = 'F'.
      wa_doc_chng-doc_size = v_lines_txt * 255.

* wa_doc_chng-doc_size=( v_lines_txt - 1 ) * 255 + strlen( i_objtxt )

      CLEAR i_objpack-transf_bin.
      i_objpack-head_start = 1.
      i_objpack-head_num = 0.
      i_objpack-body_start = 1.
      i_objpack-body_num = v_lines_txt.
      i_objpack-doc_type = 'RAW'.
      APPEND i_objpack.


* (pdf-Attachment)

      i_objpack-transf_bin = 'X'.
      i_objpack-head_start = 1.
      i_objpack-head_num = 0.
      i_objpack-body_start = 1.


*  DESCRIBE TABLE i_objbin LINES v_lines_bin.
*  READ TABLE i_objbin INDEX v_lines_bin.
*  i_objpack-doc_size = v_lines_bin * 255 .

      DESCRIBE TABLE t_objhex LINES v_lines_bin.
      READ TABLE t_objhex INTO w_objhex INDEX v_lines_bin.
      i_objpack-doc_size = v_lines_bin * 255 .

      i_objpack-body_num = v_lines_bin.
      i_objpack-doc_type = 'PDF'.
      i_objpack-obj_name = 'smart'.
      CONCATENATE 'Comprobante de Retención de IVA'
                  '- CYBERLUX'
      INTO i_objpack-obj_descr SEPARATED BY space.  "--> Tit. Adjunto
      APPEND i_objpack.

* ---> Destinatario del correo
      LOOP AT lt_destinatario INTO wa_destinatario.
        CLEAR i_reclist.
*        i_reclist-receiver = destinatario. "--> Dir. del destinatario
        i_reclist-receiver = wa_destinatario-smtp_addr.
        i_reclist-rec_type = 'U'.
        i_reclist-notif_del = 'X'.   "Se espera acuse de recibo
        i_reclist-notif_read = 'X'.  "Se espera confirmación de lectura
        i_reclist-notif_ndel = 'X'.  "Se espera acuse de recibo
        APPEND i_reclist.

        MOVE 'X' TO v_commit_work.

* ---> Envio de correo al destinatario
        CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
          EXPORTING
            document_data              = wa_doc_chng
            put_in_outbox              = 'X'
            sender_address             = remitente  "--> Direccion  del
            sender_address_type        = 'SMTP'     "Remitente del email
            commit_work                = v_commit_work
          IMPORTING
            sent_to_all                = zsend_to_all
            new_object_id              = znew_order
          TABLES
            packing_list               = i_objpack
            object_header              = wa_objhead
*         contents_bin               = i_objbin
            contents_txt               = i_objtxt
            contents_hex               = t_objhex   " contents_hex
*         object_para                = object_para
*         object_parb                = object_parb
            receivers                  = i_reclist
          EXCEPTIONS
            too_many_receivers         = 1
            document_not_sent          = 2
            document_type_not_exist    = 3
            operation_no_authorization = 4
            parameter_error            = 5
            x_error                    = 6
            enqueue_error              = 7
            OTHERS                     = 8.

        IF sy-subrc <> 0.
          ent_retco = sy-subrc.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          MESSAGE 'Correo(s) enviado(s)' TYPE 'S'.
        ENDIF.

*        CLEAR: ti_salida_mail, ti_cont_mail.
*        FREE:  ti_salida_mail, ti_cont_mail.
*        CLEAR: wa_destinatario.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

* Instructs mail send program for SAPCONNECT to send email(rsconn01)
  PERFORM initiate_mail_execute_program.

ENDFORM.                    " ENVIAR_CORREOS

*&---------------------------------------------------------------------*
*&      Form  BUSCAR_DESTINATARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DOC_XEKKO_LIFNR  text
*      <--P_DESTINATARIO  text
*----------------------------------------------------------------------*
FORM buscar_destinatario  USING i_lifnr.

  DATA: v_addrnumber LIKE adrc-addrnumber,
        v_lifnr      LIKE lfa1-lifnr.

*  DATA: lt_destinatario TYPE TABLE OF adr6,
  DATA: wa_destinatario TYPE adr6.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = i_lifnr
*    IMPORTING
*      output = v_lifnr.
*
*  SELECT SINGLE adrnr INTO v_addrnumber
*  FROM lfa1
*  WHERE lifnr EQ v_lifnr.


*  SELECT SINGLE smtp_addr INTO e_destinatario
*  FROM adr6
*  WHERE addrnumber EQ v_addrnumber
*    AND smtp_addr NE space.

*  SELECT * FROM adr6
*    INTO TABLE lt_destinatario
*    WHERE addrnumber EQ v_addrnumber
*    AND smtp_addr NE space.

*  LOOP AT lt_destinatario INTO wa_destinatario.
*    IF wa_destinatario IS NOT INITIAL.
*      e_destinatario = wa_destinatario-smtp_addr.
*    ENDIF.
*    clear: wa_destinatario.
*  ENDLOOP.



ENDFORM.                    " BUSCAR_DESTINATARIO

*&---------------------------------------------------------------------*
*&      Form  BUSCAR_REMITENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DOC_XEKKO_LIFNR  text
*      <--P_REMITENTE  text
*----------------------------------------------------------------------*
FORM buscar_remitente CHANGING e_remitente LIKE adr6-smtp_addr.

  DATA: v_addrnumber LIKE adr6-addrnumber,
        v_persnumber LIKE adr6-persnumber.

  SELECT SINGLE addrnumber persnumber
  INTO (v_addrnumber,v_persnumber)
  FROM usr21
  WHERE bname EQ syst-uname.

  SELECT SINGLE smtp_addr INTO e_remitente
  FROM adr6
  WHERE addrnumber EQ v_addrnumber
    AND persnumber EQ v_persnumber
    AND smtp_addr NE space.

ENDFORM.                    " BUSCAR_REMITENTE

*&---------------------------------------------------------------------*
*&      Form  INITIATE_MAIL_EXECUTE_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initiate_mail_execute_program .
  WAIT UP TO 1 SECONDS.
  SUBMIT rsconn01 WITH mode = 'INT'
*                WITH output = 'X'
                AND RETURN.
ENDFORM.                    " INITIATE_MAIL_EXECUTE_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  LLENAR_TABLA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM llenar_tabla_alv .

  DATA t_bseg_1 LIKE bseg OCCURS 0 WITH HEADER LINE.
  DATA v_augbl  LIKE bseg-augbl.

* Busca los documentos de compensación
  SELECT *
  INTO TABLE t_bseg_1
  FROM bseg
  FOR ALL ENTRIES IN ti_salida
  WHERE bukrs EQ ti_salida-bukrs
    AND belnr EQ ti_salida-belnr
*    AND gjahr EQ ti_salida-gjahr
    AND koart EQ 'K'
    AND augbl NE space.

* Tabla de datos del ALV
  LOOP AT ti_salida.

    MOVE-CORRESPONDING ti_salida TO ti_salida_alv.
    CLEAR ti_bkpf.
    READ TABLE ti_bkpf WITH KEY bukrs = ti_salida-bukrs
                                belnr = ti_salida-belnr.
    ti_salida_alv-gjahr = ti_bkpf-gjahr.
    IF ti_bkpf-tcode EQ 'MR8M' OR
       ti_bkpf-stblg NE space.
      ti_salida_alv-icono = '@0W@'.  " Cancel
    ELSE.
*    Busca el documento compensatorio
      READ TABLE t_bseg_1 WITH KEY bukrs = ti_salida-bukrs
                                   belnr = ti_salida-belnr
                                   gjahr = ti_bkpf-gjahr. "123
      IF t_bseg_1-augbl IS INITIAL.
        ti_salida_alv-icono = '@0V@'.  " OK
      ELSE.
*      Con este documento busca todos los documentos compensados
*      por el y luego chequea si entre uno de ellos el esta
*      anulado
        MOVE t_bseg_1-augbl TO v_augbl.
        LOOP AT t_bseg_1 WHERE augbl EQ v_augbl.
          READ TABLE ti_bkpf WITH KEY bukrs = t_bseg_1-bukrs
                                      belnr = t_bseg_1-belnr.
          IF ti_bkpf-tcode EQ 'MR8M' OR
             ti_bkpf-stblg NE space.
            ti_salida_alv-icono = '@0W@'.  " Cancel
          ENDIF.
        ENDLOOP.
*       Si no lo consigue anulado, los pone ok
        IF ti_salida_alv-icono IS INITIAL.
          ti_salida_alv-icono = '@0V@'.  " OK
        ENDIF.
      ENDIF.
    ENDIF.

*   Monto exento

    READ TABLE excento_iva WITH KEY belnr = ti_salida-belnr.
    ti_salida_alv-wrbtr1 = excento_iva-wrbtr1.
*  Compras total
    ti_salida_alv-wrbtr = ti_salida_alv-wrbtr1 + ti_salida_alv-wrbtr2 +
                          ti_salida_alv-wrbtr3.

* eliminar montos de los documentos anulados
    IF ti_bkpf-stblg NE space.
      CLEAR: ti_salida_alv-wrbtr,
             ti_salida_alv-wrbtr1,
             ti_salida_alv-wrbtr2,
             ti_salida_alv-qsatz,
             ti_salida_alv-wrbtr3,
             ti_salida_alv-wrbtr4.
    ENDIF.
    APPEND ti_salida_alv.
    CLEAR  ti_salida_alv.
  ENDLOOP.
ENDFORM.                    " LLENAR_TABLA_ALV

*&---------------------------------------------------------------------*
*&      Form  BUSCAR_FACTURA_AFECTADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_BSEG_BUKRS  text
*      -->P_WA_BSEG_REBZG  text
*      -->P_WA_BSEG_REBZJ  text
*      <--P_WA_ALV_FACAFE  text
*----------------------------------------------------------------------*
FORM buscar_factura_afectada  USING    p_bukrs
                                       p_rebzg
                                       p_rebzj
                              CHANGING p_facafe.

* Busca la factura afectada
  SELECT SINGLE xblnr INTO p_facafe
  FROM bkpf
  WHERE bukrs EQ p_bukrs
    AND belnr EQ p_rebzg
    AND gjahr EQ p_rebzj.

ENDFORM.                    " BUSCAR_FACTURA_AFECTADA

INCLUDE zrutinas_generales_01.
