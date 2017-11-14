*&---------------------------------------------------------------------*
*& Report ZPPO_PROGR_PRODUCCION
*&---------------------------------------------------------------------*
*& XCODGARCIA1 24.06.2015 PROYECTO GO!
*&---------------------------------------------------------------------*
REPORT zppo_progr_produccion NO STANDARD PAGE
HEADING LINE-SIZE 285 LINE-COUNT 65.
INCLUDE zppo_progr_produccion_top.

AT SELECTION-SCREEN ON so_gstrs.
 PERFORM fo_validar_fechas.
AT SELECTION-SCREEN ON pa_nivel.
 PERFORM fo_validar_niveles.

START-OF-SELECTION.
 PERFORM fo_seleccionar_datos.
END-OF-SELECTION.
 PERFORM fo_mostrar_seleccion. 

*&---------------------------------------------------------------------*
*&  Include           ZPPO_PROGR_PRODUCCION_TYP
*&---------------------------------------------------------------------*
* información de una orden
TYPES: BEGIN OF ty_order_info,
 aufnr TYPE caufv-aufnr, "número de orden
 auart TYPE caufv-auart, "clase de orden
 posnr TYPE afpo-posnr,  "posición
 werks TYPE caufv-werks, "centro
 mdv01 TYPE mkal-mdv01,  "línea
 matnr TYPE caufv-plnbez, "material
 verid TYPE afpo-verid,  "versión de fabricación
 gstrs TYPE caufv-gstrs, "inicio (fecha)
 gltrs TYPE caufv-gltrs, "fin (fecha)
 rdate TYPE caufv-gstrs, "inicio (fecha real)
 gsuzs TYPE caufv-gsuzs, "inicio (hora)
 gluzs TYPE caufv-gluzs, "fin (hora)
 gamng TYPE caufv-gamng, "cantidad plan
 wemng TYPE afpo-wemng,  "cantidad real
 meins TYPE caufv-gmein.
TYPES: END OF ty_order_info.
* cabecera de necesid. capacid.
TYPES: BEGIN OF ty_kbko,
 bedid TYPE kbko-bedid,
 typkz TYPE kbko-typkz,
 aufpl TYPE kbko-aufpl,
 plnum TYPE kbko-plnum,
 gstrs TYPE kbko-gstrs,
 gltrs TYPE kbko-gltrs,
 gsuzs TYPE kbko-gsuzs,
 gluzs TYPE kbko-gluzs.
TYPES: END OF ty_kbko.
* ordenes previsionales
TYPES: BEGIN OF ty_plaf,
 plnum TYPE plaf-plnum,
 paart TYPE plaf-paart,
 pwwrk TYPE plaf-pwwrk,
 mdv01 TYPE mkal-mdv01,
 matnr TYPE plaf-matnr,
 verid TYPE plaf-verid,
 gstrs TYPE kbko-gstrs,
 gltrs TYPE kbko-gltrs,
 gsuzs TYPE kbko-gsuzs,
 gluzs TYPE kbko-gluzs,
 gsmng TYPE plaf-gsmng,
 meins TYPE plaf-meins.
TYPES: END OF ty_plaf.
* registros de vista de ordenes
TYPES: BEGIN OF ty_cauf,
 aufnr  TYPE caufv-aufnr,
 auart  TYPE caufv-auart,
 objnr  TYPE caufv-objnr,
 werks  TYPE caufv-werks,
 sowrk  TYPE caufv-sowrk,
 mdv01  TYPE mkal-mdv01,
 plnbez TYPE caufv-plnbez,
 verid  TYPE afpo-verid,
 gstrs  TYPE caufv-gstrs,
 gltrs  TYPE caufv-gltrs,
 gsuzs  TYPE caufv-gsuzs,
 gluzs  TYPE caufv-gluzs,
 gamng  TYPE caufv-gamng,
 wemng  TYPE afpo-wemng,
 gmein  TYPE caufv-gmein.
TYPES: END OF ty_cauf.
* versiones de fabricación del material
TYPES: BEGIN OF ty_mkal,
 matnr TYPE mkal-matnr,
 werks TYPE mkal-werks,
 verid TYPE mkal-verid,
 mdv01 TYPE mkal-mdv01.
TYPES: END OF ty_mkal.
* registros de necesidades de capacidad
TYPES: BEGIN OF ty_kbed,
 plnum TYPE kbed-plnum,
 sstau TYPE kbed-sstau.
TYPES: END OF ty_kbed.
* registros de posición de la orden
TYPES: BEGIN OF ty_afpo,
 aufnr TYPE afpo-aufnr,
 posnr TYPE afpo-posnr,
 matnr TYPE afpo-matnr,
 pwerk TYPE afpo-pwerk,
 verid TYPE afpo-verid,
 psmng TYPE afpo-psmng,
 wemng TYPE afpo-wemng,
 amein TYPE afpo-amein.
TYPES: END OF ty_afpo.
* registros cab. orden mantenimiento
TYPES: BEGIN OF ty_afih,
 aufnr TYPE afih-aufnr,
 iloan TYPE afih-iloan.
TYPES: END OF ty_afih.
* números de materiales
TYPES: BEGIN OF ty_mara,
 matnr TYPE mara-matnr,
 mtart TYPE mara-mtart.
TYPES: END OF ty_mara.
* descripción de materiales
TYPES: BEGIN OF ty_makt,
 matnr TYPE makt-matnr,
 maktx TYPE makt-maktx.
TYPES: END OF ty_makt.
* textos de las líneas
TYPES: BEGIN OF ty_crtx,
 arbpl TYPE crhd-arbpl,
 ktext TYPE crtx-ktext.
TYPES: END OF ty_crtx.
* puestos de trabajo
TYPES: BEGIN OF ty_crhd,
 aufnr TYPE afih-aufnr,
 arbpl TYPE crhd-arbpl,
 werks TYPE crhd-werks.
TYPES: END OF ty_crhd.
* factores conversión U/M
TYPES: BEGIN OF ty_fact,
 matnr  TYPE matnr,
 nach   TYPE meins,
 von    TYPE meins,
 factor TYPE float.
TYPES: END OF ty_fact. 

*&---------------------------------------------------------------------*
*&  Include           ZPPO_PROGR_PRODUCCION_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS: rsds, slis, icon.
* tablas utilizadas
TABLES: kbko, plaf, mkal, kbed, marm, makt, caufv, usr01.
* parámetros de selección
INCLUDE zppo_progr_produccion_scr.
* definición e implementación de clases
INCLUDE zppo_progr_produccion_lcl.
* objeto reporte de producción
DATA go_reporte TYPE REF TO lcl_reporte_produccion.
* subrutinas del programa
INCLUDE zppo_progr_produccion_i01.
* variable código de interacción
DATA ok_code TYPE sy-ucomm.
* variable información
DATA screen_header(164).
* modulos PAI y PBO
INCLUDE zppo_progr_produccion_pbo.
INCLUDE zppo_progr_produccion_pai. 

*&---------------------------------------------------------------------*
*&  Include           ZPPO_PROGR_PRODUCCION_SCR
*&---------------------------------------------------------------------*
* Parámetros de Selección.
SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE text-b01.
PARAMETERS:
pa_werks TYPE caufv-werks OBLIGATORY. "Centro
SELECT-OPTIONS:
so_werks FOR caufv-werks NO-DISPLAY,
so_mdv01 FOR mkal-mdv01  NO-EXTENSION, "Linea de Producción
so_matnr FOR caufv-plnbez, "Material
so_dispo FOR caufv-dispo NO-EXTENSION, "Planificador
so_gstrs FOR caufv-gstrs NO-EXTENSION OBLIGATORY. "Período
SELECTION-SCREEN END OF BLOCK bl01.
* opciones
SELECTION-SCREEN BEGIN OF BLOCK bl03 WITH FRAME TITLE text-b03.
SELECT-OPTIONS:
"Clase de orden contingencia
so_cotyp FOR caufv-auart NO INTERVALS OBLIGATORY,
so_cousr FOR caufv-ernam NO-DISPLAY NO INTERVALS
MATCHCODE OBJECT mst_elm_user_name,
"Clase de orden planificada
so_potyp FOR caufv-auart NO INTERVALS OBLIGATORY,
so_pousr FOR caufv-ernam NO INTERVALS
MATCHCODE OBJECT mst_elm_user_name,
"Clase de orden mantenimiento
so_motyp FOR caufv-auart NO INTERVALS OBLIGATORY DEFAULT 'PM03',
so_mousr FOR caufv-ernam NO-DISPLAY NO INTERVALS
MATCHCODE OBJECT mst_elm_user_name.
SELECTION-SCREEN END OF BLOCK bl03.
* opciones de selección de turnos
SELECTION-SCREEN BEGIN OF BLOCK bl04 WITH FRAME TITLE text-b04.
PARAMETERS:
pa_meins TYPE mara-meins OBLIGATORY DEFAULT 'CS', "U/M: cajas por defecto
rb_mdv01 RADIOBUTTON GROUP rb DEFAULT 'X',
rb_matnr RADIOBUTTON GROUP rb,
pa_calen AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl04.
* nivel de desglose en fondo
SELECTION-SCREEN BEGIN OF BLOCK bl02 WITH FRAME TITLE text-b02.
PARAMETERS:
pa_nivel(1) TYPE n DEFAULT '2' OBLIGATORY,
pa_maxds TYPE i NO-DISPLAY DEFAULT 31. "Máximo número de días a reportar
SELECTION-SCREEN END OF BLOCK bl02. 

*&---------------------------------------------------------------------*
*&  Include           ZPPO_PROGR_PRODUCCION_LCL
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_reporte_produccion
*----------------------------------------------------------------------*
CLASS lcl_reporte_produccion DEFINITION FINAL.

 PUBLIC SECTION.
*   definición de tipos de datos
   INCLUDE zppo_progr_produccion_typ.
*   tablas públicas
   CLASS-DATA gt_order_info_plan TYPE STANDARD TABLE OF ty_order_info.
   CLASS-DATA gt_order_info_real TYPE STANDARD TABLE OF ty_order_info.
*   referencias a las tablas dinámicas
   CLASS-DATA gt_ref_plan TYPE REF TO data.
   CLASS-DATA gt_ref_real TYPE REF TO data.
*   log de mensajes
   CLASS-DATA gt_log TYPE upc_yt_mesg.
*   variables de selección
   CLASS-DATA gr_werks TYPE RANGE OF caufv-werks.
   CLASS-DATA gr_gstrs TYPE RANGE OF caufv-gstrs.
   CLASS-DATA gs_gstrs LIKE LINE OF gr_gstrs.
   CLASS-DATA gr_mdv01 TYPE RANGE OF mkal-mdv01.
   CLASS-DATA gr_matnr TYPE RANGE OF mara-matnr.
   CLASS-DATA gr_dispo TYPE RANGE OF caufv-dispo.
   CLASS-DATA gr_potyp TYPE RANGE OF caufv-auart.
   CLASS-DATA gr_motyp TYPE RANGE OF caufv-auart.
   CLASS-DATA gr_cotyp TYPE RANGE OF caufv-auart.
   CLASS-DATA gr_cousr TYPE RANGE OF caufv-ernam.
   CLASS-DATA gr_mousr TYPE RANGE OF caufv-ernam.
   CLASS-DATA gr_pousr TYPE RANGE OF caufv-ernam.
   CLASS-DATA gr_ernam TYPE RANGE OF caufv-ernam.
   CLASS-DATA gr_auart TYPE RANGE OF caufv-auart.
   CLASS-DATA gp_meins TYPE mara-meins.
   CLASS-DATA gp_nivel TYPE n.
   CLASS-DATA gp_calen.
   CLASS-DATA gp_sku.
*   métodos públicos
   CLASS-METHODS inicializar_gstrs.
   CLASS-METHODS inicializar_datos.
   CLASS-METHODS seleccionar_datos.
   CLASS-METHODS impresion_reporte.
   CLASS-METHODS validar_autorizaciones.
*   método contructor
   METHODS constructor
   IMPORTING i_werks LIKE gr_werks
             i_gstrs LIKE gr_gstrs
             i_mdv01 LIKE gr_mdv01
             i_matnr LIKE gr_matnr
             i_dispo LIKE gr_dispo
             i_potyp LIKE gr_potyp
             i_motyp LIKE gr_motyp
             i_cotyp LIKE gr_cotyp
             i_pousr LIKE gr_pousr
             i_cousr LIKE gr_cousr
             i_mousr LIKE gr_mousr
             i_meins LIKE gp_meins
             i_calen LIKE gp_calen
             i_nivel LIKE gp_nivel
             i_sku   LIKE gp_sku.

 PRIVATE SECTION.
*   constantes
   CONSTANTS co_fabricacion_propia TYPE plaf-beskz VALUE 'E'.
   CONSTANTS co_orden_almacen TYPE plaf-paart VALUE 'LA'.
   CONSTANTS co_producto_terminado TYPE mara-mtart VALUE 'FERT'.
   CONSTANTS co_peticion_borrado(4) VALUE 'PTBO'.
   CONSTANTS co_6am TYPE atime VALUE '060000'.
   CONSTANTS co_12am TYPE atime VALUE '000000'.
   CONSTANTS co_2359 TYPE atime VALUE '235959'.
   CONSTANTS co_decimales TYPE i VALUE 3.
*   campos clave (niveles) del reporte
   CONSTANTS co_key_1 TYPE lvc_s_fcat-ref_field VALUE 'MDV01'. "línea
   CONSTANTS co_key_2 TYPE lvc_s_fcat-ref_field VALUE 'MATNR'. "material
   CONSTANTS co_key_3 TYPE lvc_s_fcat-ref_field VALUE 'POSNR'. "posición
   CONSTANTS co_label TYPE lvc_s_fcat-ref_field VALUE 'LABEL'. "etiqueta
   CONSTANTS co_total TYPE lvc_s_fcat-ref_field VALUE 'TOTAL'. "totales
   CONSTANTS co_plan  TYPE lvc_s_fcat-ref_field VALUE 'GAMNG'. "cantidad plan
   CONSTANTS co_real  TYPE lvc_s_fcat-ref_field VALUE 'WEMNG'. "cantidad real
*   indicador de autorización: visualización de cuadro real
   CLASS-DATA gi_real TYPE xfeld.
*   objetos contenedores
   CLASS-DATA go_container TYPE REF TO cl_gui_custom_container.
   CLASS-DATA go_splitter TYPE REF TO cl_gui_splitter_container.
   CLASS-DATA go_container_plan TYPE REF TO cl_gui_container.
   CLASS-DATA go_container_real TYPE REF TO cl_gui_container.
*   objetos árbol
   CLASS-DATA go_alv_plan TYPE REF TO cl_salv_tree.
   CLASS-DATA go_alv_real TYPE REF TO cl_salv_tree.
*   tablas internas
   CLASS-DATA it_kbko TYPE STANDARD TABLE OF ty_kbko.
   CLASS-DATA it_plaf TYPE STANDARD TABLE OF ty_plaf.
   CLASS-DATA it_mkal TYPE STANDARD TABLE OF ty_mkal.
   CLASS-DATA it_crhd TYPE STANDARD TABLE OF ty_crhd.
   CLASS-DATA it_kbed TYPE STANDARD TABLE OF ty_kbed.
   CLASS-DATA it_afpo TYPE STANDARD TABLE OF ty_afpo.
   CLASS-DATA it_afih TYPE STANDARD TABLE OF ty_afih.
   CLASS-DATA it_mara TYPE STANDARD TABLE OF ty_mara.
   CLASS-DATA it_makt TYPE STANDARD TABLE OF ty_makt.
   CLASS-DATA it_crtx TYPE STANDARD TABLE OF ty_crtx.
   CLASS-DATA it_cauf TYPE STANDARD TABLE OF ty_cauf.
   CLASS-DATA it_fact TYPE STANDARD TABLE OF ty_fact.
*   catálogo de campos de la tabla de salida
   CLASS-DATA it_fieldcat TYPE lvc_t_fcat.
*   métodos privados
   CLASS-METHODS autorizacion_werks.
   CLASS-METHODS autorizacion_real.
   CLASS-METHODS inicializar_auart
       IMPORTING i_real TYPE xfeld.
   CLASS-METHODS inicializar_ernam
       IMPORTING i_real TYPE xfeld.
   CLASS-METHODS ordenes_previsionales.
   CLASS-METHODS ordenes_mantenimiento.
   CLASS-METHODS ordenes_proceso
       IMPORTING i_real TYPE xfeld.
   CLASS-METHODS sort_order_info
       IMPORTING i_real TYPE xfeld.
   CLASS-METHODS append_order_info
       IMPORTING i_real TYPE xfeld
        CHANGING c_info TYPE ty_order_info.
   CLASS-METHODS producto_terminado
       IMPORTING i_matnr TYPE matnr
       RETURNING VALUE(r) TYPE xfeld.
   CLASS-METHODS orden_borrada
       IMPORTING i_objnr TYPE caufv-objnr
       RETURNING VALUE(r) TYPE xfeld.
   CLASS-METHODS tipo_material
       IMPORTING i_matnr TYPE matnr
       RETURNING VALUE(r_mtart) TYPE mara-mtart.
   CLASS-METHODS nombre_material
       IMPORTING i_matnr TYPE matnr
       RETURNING VALUE(r_maktx) TYPE makt-maktx.
   CLASS-METHODS texto_linea
       IMPORTING i_arbpl TYPE arbpl
       RETURNING VALUE(r_ktext) TYPE crtx-ktext.
   CLASS-METHODS cantidades_afpo
       IMPORTING i_cauf LIKE LINE OF it_cauf
       RETURNING VALUE(r_cauf) LIKE LINE OF it_cauf.
   CLASS-METHODS fecha_ordenamiento
        CHANGING c_info TYPE ty_order_info.
   CLASS-METHODS verificar_unidades
        CHANGING c_info TYPE ty_order_info.
   CLASS-METHODS verificar_split
       IMPORTING i_real TYPE xfeld
        CHANGING c_info TYPE ty_order_info.
   CLASS-METHODS split_cantidad
       IMPORTING i_ini TYPE caufv-gluzs
                 i_fin TYPE caufv-gluzs
       EXPORTING e_cant_1 TYPE caufv-gamng
                 e_cant_2 TYPE caufv-gamng.
   CLASS-METHODS numero_containers
       RETURNING VALUE(r) TYPE i.
*   metodos creación de tabla dinámica
   CLASS-METHODS create_field_catalog.
   CLASS-METHODS create_dynamic_tables.
   CLASS-METHODS create_dynamic_table
        CHANGING c_ref TYPE REF TO data
                 c_alv_ref TYPE REF TO cl_salv_tree
                 c_gui_ref TYPE REF TO cl_gui_container.
   CLASS-METHODS fill_dynamic_tables.
   CLASS-METHODS fill_dynamic_table
       IMPORTING i_real TYPE xfeld.
   CLASS-METHODS fill_dynamic_table_sku
       IMPORTING i_real TYPE xfeld.
   CLASS-METHODS calculate_totals
        CHANGING c_table_ref TYPE REF TO data.
   CLASS-METHODS calculate_totals_sku
        CHANGING c_table_ref TYPE REF TO data.
   CLASS-METHODS texto_auart
       IMPORTING i_auart TYPE auart
       RETURNING VALUE(r_text) TYPE itex132.
   CLASS-METHODS texto_meins
       IMPORTING i_meins TYPE meins
       RETURNING VALUE(r_text) TYPE t006a-mseht.
   CLASS-METHODS format_aufnr
       IMPORTING i_aufnr TYPE any
       RETURNING VALUE(r_aufnr) TYPE aufnr.
   CLASS-METHODS format_matnr
       IMPORTING i_matnr TYPE any
       RETURNING VALUE(r_matnr) TYPE matnr.
   CLASS-METHODS string_to_number
       IMPORTING i_string TYPE any
       RETURNING VALUE(r_value) TYPE i.
   CLASS-METHODS format_number
       IMPORTING i_value TYPE any
       RETURNING VALUE(r_string) TYPE string.
   CLASS-METHODS time_to_string
       IMPORTING i_time TYPE any
       RETURNING VALUE(r_time) TYPE string.
   CLASS-METHODS factor_conversion
       IMPORTING i_matnr TYPE matnr
                 i_de TYPE meins
                 i_a TYPE meins
       RETURNING VALUE(r_fact) TYPE f.
*   metodos ALV tree
   CLASS-METHODS show_alv_trees.
   CLASS-METHODS show_alv_tree
       IMPORTING i_text TYPE itex132
        CHANGING c_alv_ref TYPE REF TO cl_salv_tree.
   CLASS-METHODS create_alv_tree
        CHANGING c_table_ref TYPE REF TO data.
   CLASS-METHODS create_nodes_alv_trees.
   CLASS-METHODS create_nodes_alv_tree
       IMPORTING i_real TYPE xfeld.
   CLASS-METHODS create_nodes_alv_tree_sku
       IMPORTING i_real TYPE xfeld.
   CLASS-METHODS setup_alv_tree
       IMPORTING i_real TYPE xfeld
        CHANGING c_alv_ref TYPE REF TO cl_salv_tree.
   CLASS-METHODS setup_alv_events
       IMPORTING i_real TYPE xfeld
        CHANGING c_alv_ref TYPE REF TO cl_salv_tree.
   CLASS-METHODS on_double_click_real
       FOR EVENT double_click OF cl_salv_events_tree
       IMPORTING node_key columnname.
   CLASS-METHODS on_double_click_plan
       FOR EVENT double_click OF cl_salv_events_tree
       IMPORTING node_key columnname.
   CLASS-METHODS get_alv_row_style
       IMPORTING i_count TYPE i
       RETURNING VALUE(r_style) TYPE salv_de_constant.
   CLASS-METHODS show_order
       IMPORTING i_real TYPE xfeld
                 i_node_key TYPE salv_de_node_key
                 i_columnname TYPE lvc_fname.
ENDCLASS.

CLASS lcl_reporte_produccion IMPLEMENTATION.
*&---------------------------------------------------------------------*
*& Public Method CONSTRUCTOR
*&---------------------------------------------------------------------*
 METHOD constructor.
*   opciones de selección
   gr_werks[] = i_werks[].
   gr_gstrs[] = i_gstrs[].
   gr_mdv01[] = i_mdv01[].
   gr_matnr[] = i_matnr[].
   gr_dispo[] = i_dispo[].
   gr_cotyp[] = i_cotyp[].
   gr_potyp[] = i_potyp[].
   gr_motyp[] = i_motyp[].
   gr_cousr[] = i_cousr[].
   gr_pousr[] = i_pousr[].
   gr_mousr[] = i_mousr[].
*   unidad de medida de visualización
   gp_meins = i_meins.
*   niveles del reporte
   gp_nivel = i_nivel.
*   tipo de turno
   gp_calen = i_calen.
*   reporte desglosado por SKU
   gp_sku = i_sku.
*   inicializa datos
   CALL METHOD inicializar_datos.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Public Method INICIALIZAR_DATOS
*&---------------------------------------------------------------------*
 METHOD inicializar_datos.
   CALL METHOD inicializar_gstrs.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Public Method SELECCIONAR_DATOS
*&---------------------------------------------------------------------*
 METHOD seleccionar_datos.
   CALL METHOD ordenes_previsionales.
   CALL METHOD ordenes_proceso EXPORTING i_real = ' '. "cuadro PLAN
   CALL METHOD ordenes_proceso EXPORTING i_real = 'X'. "cuadro REAL
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Public Method IMPRESION_REPORTE
*&---------------------------------------------------------------------*
 METHOD impresion_reporte.
*   solo ejecuta la primera vez
   CHECK go_container IS NOT BOUND.
*   crea el catálogo de campos de las tablas
   CALL METHOD create_field_catalog.
*   creación de las tablas dinámicas
   CALL METHOD create_dynamic_tables.
*   llena las tablas dinámicas
   CALL METHOD fill_dynamic_tables.
*   creación de los nodos de la tablas
   CALL METHOD create_nodes_alv_trees.
*   impresión del alv tree
   CALL METHOD show_alv_trees.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Public Method VALIDAR_AUTORIZACIONES
*&---------------------------------------------------------------------*
 METHOD validar_autorizaciones.
   CALL METHOD autorizacion_werks. "centro y clases de orden
   CALL METHOD autorizacion_real. "determina autorización para vis.REAL
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method AUTORIZACION_WERKS
*&---------------------------------------------------------------------*
 METHOD autorizacion_werks.
   DATA: lt_t001w TYPE STANDARD TABLE OF t001w.
   FIELD-SYMBOLS <fs_t001w> LIKE LINE OF lt_t001w.
   SELECT werks FROM t001w
     INTO CORRESPONDING FIELDS OF TABLE lt_t001w
     WHERE werks IN gr_werks.
   LOOP AT lt_t001w ASSIGNING <fs_t001w>.
     AUTHORITY-CHECK OBJECT 'C_AFKO_AWK'
         ID 'WERKS'  FIELD <fs_t001w>-werks
         ID 'AUFART' DUMMY.
     "no tiene autorización para el centro &
     IF sy-subrc IS NOT INITIAL.
       MESSAGE e004(co) WITH <fs_t001w>-werks.
     ENDIF.
   ENDLOOP.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method AUTORIZACION_REAL
*&---------------------------------------------------------------------*
 METHOD autorizacion_real.
*   verfica si tiene autorización para visualizar
*   el cuadro de datos de producción real
*    AUTHORITY-CHECK OBJECT '???????'
*        ID '????' FIELD ???????
*        ID '????' DUMMY.
   "si está autorizado
   IF sy-subrc IS INITIAL.
      gi_real = 'X'.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method INICIALIZAR_GSTRS
*&---------------------------------------------------------------------*
 METHOD inicializar_gstrs.
   READ TABLE gr_gstrs INDEX 1 INTO gs_gstrs.
   IF gs_gstrs-high IS INITIAL.
      gs_gstrs-high = gs_gstrs-low.
      gs_gstrs-option = 'BT'.
      MODIFY gr_gstrs FROM gs_gstrs INDEX 1.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method INICIALIZAR_AUART
*&---------------------------------------------------------------------*
 METHOD inicializar_auart.
   CLEAR gr_auart.
*   rango de clases de ordenes
   IF i_real EQ 'M'. "ordenes de mantenimiento
     APPEND LINES OF gr_motyp TO gr_auart.
   ELSE.
     APPEND LINES OF gr_potyp TO gr_auart.
*     para el cuadro real se incluyen ordenes
*     de clase contingencia
     IF i_real IS NOT INITIAL.
     APPEND LINES OF gr_cotyp TO gr_auart.
     ENDIF.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method INICIALIZAR_ERNAM
*&---------------------------------------------------------------------*
 METHOD inicializar_ernam.
   CLEAR gr_ernam.
*   NOTA: para el negocio de CYM, el cuadro plan debe
*   incluir solo ordenes creadas por usuario determinados
   IF i_real IS NOT INITIAL AND gr_pousr IS NOT INITIAL.
   APPEND LINES OF gr_pousr TO gr_ernam.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method ORDENES_PREVISIONALES
*&---------------------------------------------------------------------*
 METHOD ordenes_previsionales.

   DATA: ls_info TYPE ty_order_info.
   DATA: lt_plaf TYPE STANDARD TABLE OF ty_plaf.
   FIELD-SYMBOLS <fs_plaf> LIKE LINE OF it_plaf.
   FIELD-SYMBOLS <fs_mkal> LIKE LINE OF it_mkal.
   FIELD-SYMBOLS <fs_kbko> LIKE LINE OF it_kbko.

   CLEAR: it_kbko, it_plaf, it_mkal, it_kbed.
*   registro cabecera de necesid. capacid.
   SELECT bedid typkz aufpl plnum gstrs gltrs gsuzs gluzs
     FROM kbko
     INTO CORRESPONDING FIELDS OF TABLE it_kbko
     WHERE typkz EQ '2' "orden previsional
       AND plnty EQ '2' "receta de planificación
       AND gstrs IN gr_gstrs.
   CHECK it_kbko[] IS NOT INITIAL.
*   lee ordenes provisionales del plan de produccion
   SELECT plnum paart matnr pwwrk gsmng psttr verid meins
     FROM plaf
     INTO CORRESPONDING FIELDS OF TABLE it_plaf
     FOR ALL ENTRIES IN it_kbko
    WHERE plnum EQ it_kbko-plnum
      AND matnr IN gr_matnr
      AND pwwrk IN gr_werks
      AND dispo IN gr_dispo
      AND paart EQ co_orden_almacen
*      AND auffx IN so_auffx "indicador de fijación para datos de la orden previsional
      AND beskz = co_fabricacion_propia
      AND obart = '1'. "clase de objeto
*      AND psttr IN so_gstrs
*      AND trmkz IN so_trmkz.
*   descarta las ordenes de materiales que no sean productos terminados.
   LOOP AT it_plaf ASSIGNING <fs_plaf>.
     IF producto_terminado( <fs_plaf>-matnr ) IS NOT INITIAL.
       INSERT <fs_plaf> INTO TABLE lt_plaf.
     ENDIF.
   ENDLOOP.
   CHECK lt_plaf IS NOT INITIAL.
*   Obtiene las líneas de producción
   SELECT matnr werks verid mdv01
     INTO CORRESPONDING FIELDS OF TABLE it_mkal
     FROM mkal
     FOR ALL ENTRIES IN lt_plaf
     WHERE matnr EQ lt_plaf-matnr
       AND werks EQ lt_plaf-pwwrk
       AND verid EQ lt_plaf-verid
       AND mdv01 IN gr_mdv01.
*   elimina los registros con líneas vacías
   DELETE it_mkal[] WHERE mdv01 IS INITIAL.
   CLEAR: it_plaf[].
*   registra datos adicionales.
   LOOP AT lt_plaf ASSIGNING <fs_plaf>.
*     linea de produccion
     READ TABLE it_mkal
     ASSIGNING <fs_mkal> WITH KEY
     matnr = <fs_plaf>-matnr
     werks = <fs_plaf>-pwwrk
     verid = <fs_plaf>-verid.
     CHECK sy-subrc EQ 0.
     <fs_plaf>-mdv01 = <fs_mkal>-mdv01.
*     hora planificada exacta de la orden
     READ TABLE it_kbko
     ASSIGNING <fs_kbko> WITH KEY
     plnum = <fs_plaf>-plnum.
     IF sy-subrc EQ 0.
       <fs_plaf>-gstrs = <fs_kbko>-gstrs.
       <fs_plaf>-gsuzs = <fs_kbko>-gsuzs.
       <fs_plaf>-gltrs = <fs_kbko>-gltrs.
       <fs_plaf>-gluzs = <fs_kbko>-gluzs.
     ENDIF.
*     actualiza los datos en la tabla
     MOVE-CORRESPONDING <fs_plaf> TO ls_info.
     MOVE <fs_plaf>-plnum TO ls_info-aufnr.
     MOVE <fs_plaf>-paart TO ls_info-auart.
     MOVE <fs_plaf>-pwwrk TO ls_info-werks.
     MOVE <fs_plaf>-gsmng TO ls_info-gamng.
     CALL METHOD append_order_info
       EXPORTING i_real = space
        CHANGING c_info = ls_info.
   ENDLOOP.
*   ordena la tabla resultante
   CALL METHOD sort_order_info EXPORTING i_real = ' '.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method ORDENES_MANTENIMIENTO
*&---------------------------------------------------------------------*
 METHOD ordenes_mantenimiento.

   DATA: ls_info TYPE ty_order_info.
   DATA: lt_cauf TYPE STANDARD TABLE OF ty_cauf.
   FIELD-SYMBOLS <fs_cauf> LIKE LINE OF it_cauf.
   FIELD-SYMBOLS <fs_crhd> LIKE LINE OF it_crhd.
   FIELD-SYMBOLS <fs_info> TYPE ty_order_info.
*   inicializa las opciones de selección de clase de orden
   CALL METHOD inicializar_auart
     EXPORTING
       i_real = 'M'.
   CLEAR: it_cauf, it_crhd.
*   Vista para cabecera orden PPS/RK: ordenes de mantenimiento
   SELECT aufnr auart sowrk objnr gstrs gltrs gsuzs gluzs gmein
     FROM caufv
     APPENDING CORRESPONDING FIELDS OF TABLE it_cauf
     WHERE auart IN gr_auart
       AND sowrk IN gr_werks
       AND gstrs IN gr_gstrs.
*   se deben descartar las órdenes que estén marcadas
*   para borrar (estatus PTBO)
   LOOP AT it_cauf ASSIGNING <fs_cauf>.
*     solo toma en cuenta ordenes no borradas
     IF orden_borrada( <fs_cauf>-objnr ) IS INITIAL.
        INSERT <fs_cauf> INTO TABLE lt_cauf.
     ENDIF.
   ENDLOOP.
   CHECK lt_cauf[] IS NOT INITIAL.
*   obtiene las líneas de producción
*   relacionadas con ordenes de mantenimiento
   SELECT a~aufnr c~arbpl c~werks
     INTO CORRESPONDING FIELDS OF TABLE it_crhd
     FROM ( iloa AS i INNER JOIN crhd AS c ON i~ppsid = c~objid AND i~cr_objty = c~objty )
     INNER JOIN afih AS a ON a~iloan = i~iloan
     FOR ALL ENTRIES IN lt_cauf
    WHERE a~aufnr = lt_cauf-aufnr.
*   elimina los registros con líneas vacías
   DELETE it_crhd[] WHERE arbpl IS INITIAL.
    CHECK it_crhd[] IS NOT INITIAL.
*   registra datos adicionales.
   LOOP AT lt_cauf ASSIGNING <fs_cauf>.
     READ TABLE it_crhd
     ASSIGNING <fs_crhd> WITH KEY
     aufnr = <fs_cauf>-aufnr
     werks = <fs_cauf>-sowrk.
     CHECK sy-subrc EQ 0.
     <fs_cauf>-mdv01 = <fs_crhd>-arbpl.
     <fs_cauf>-werks = <fs_crhd>-werks.
*     busca al material procesado en esa
*     línea de producción
     READ TABLE gt_order_info_plan
     ASSIGNING <fs_info> WITH KEY
     mdv01 = <fs_cauf>-mdv01
     werks = <fs_cauf>-werks
     gstrs = <fs_cauf>-gstrs.
     IF sy-subrc EQ 0.
     <fs_cauf>-plnbez = <fs_info>-matnr.
     ELSE. "sin material asociado
     <fs_cauf>-plnbez = text-t01. "'S/M (SIN MATERIAL)'.
     ENDIF.
*     actualiza los datos en la tabla
     MOVE-CORRESPONDING <fs_cauf> TO ls_info.
     MOVE <fs_cauf>-plnbez TO ls_info-matnr.
*     registra la orden
     CALL METHOD append_order_info
       EXPORTING i_real = ' '
        CHANGING c_info = ls_info.
   ENDLOOP.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method ORDENES_PROCESO
*&---------------------------------------------------------------------*
 METHOD ordenes_proceso.

   DATA: ls_info TYPE ty_order_info.
   DATA: lt_cauf TYPE STANDARD TABLE OF ty_cauf.
   FIELD-SYMBOLS <fs_cauf> LIKE LINE OF it_cauf.
   FIELD-SYMBOLS <fs_mkal> LIKE LINE OF it_mkal.
   FIELD-SYMBOLS <fs_afpo> LIKE LINE OF it_afpo.
*   verifica que tenga autorización p.cuadro REAL
   CHECK i_real IS INITIAL OR gi_real IS NOT INITIAL.
*   inicializa las opciones de selección de clase de orden
   CALL METHOD inicializar_auart
     EXPORTING
       i_real = i_real.
*   inicializa las opciones de selección de usuario creador
   CALL METHOD inicializar_ernam
     EXPORTING
       i_real = i_real.
   CLEAR: it_cauf, it_afpo, it_mkal.
*   Vista para cabecera orden PPS/RK: ordenes de proceso
   SELECT aufnr auart werks objnr plnbez
     gamng gmein gstrs gltrs gsuzs gluzs
     FROM caufv
     INTO CORRESPONDING FIELDS OF TABLE it_cauf
     WHERE auart  IN gr_auart
       AND ernam  IN gr_ernam
       AND werks  IN gr_werks
       AND dispo  IN gr_dispo
       AND gstrs  IN gr_gstrs
       AND plnbez IN gr_matnr.
*   se deben descartar las órdenes que estén marcadas
*   para borrar (estatus PTBO)
   LOOP AT it_cauf ASSIGNING <fs_cauf>.
*     solo toma en cuenta ordenes no borradas
     IF orden_borrada( <fs_cauf>-objnr ) IS INITIAL AND
*        y ordenes de producción con productos terminados
        producto_terminado( <fs_cauf>-plnbez ) IS NOT INITIAL.
        INSERT <fs_cauf> INTO TABLE lt_cauf.
     ENDIF.
   ENDLOOP.
   CHECK lt_cauf[] IS NOT INITIAL.
*   Posiciones de las ordenes (de producción)
   SELECT aufnr posnr matnr pwerk verid psmng wemng amein
     FROM afpo
     INTO CORRESPONDING FIELDS OF TABLE it_afpo
     FOR ALL ENTRIES IN lt_cauf
     WHERE aufnr EQ lt_cauf-aufnr.
*   Obtiene las líneas de producción
*   relacionadas con ordenes de producción
   IF it_afpo[] IS NOT INITIAL.
     SELECT matnr werks verid mdv01
       INTO CORRESPONDING FIELDS OF TABLE it_mkal
       FROM mkal
       FOR ALL ENTRIES IN it_afpo
       WHERE matnr EQ it_afpo-matnr
         AND werks EQ it_afpo-pwerk
         AND verid EQ it_afpo-verid
         AND mdv01 IN gr_mdv01.
   ENDIF.
*   elimina los registros con líneas vacías
   DELETE it_mkal[] WHERE mdv01 IS INITIAL.
    CHECK it_mkal[] IS NOT INITIAL.
*   registra datos adicionales.
   LOOP AT lt_cauf ASSIGNING <fs_cauf>.
*     busca la versión de producción
     READ TABLE it_afpo
     ASSIGNING <fs_afpo> WITH KEY
     aufnr = <fs_cauf>-aufnr
     matnr = <fs_cauf>-plnbez
     pwerk = <fs_cauf>-werks.
     CHECK sy-subrc EQ 0.
*     linea de produccion
     READ TABLE it_mkal
     ASSIGNING <fs_mkal> WITH KEY
     matnr = <fs_cauf>-plnbez
     werks = <fs_cauf>-werks
     verid = <fs_afpo>-verid.
     CHECK sy-subrc EQ 0.
     <fs_cauf>-mdv01 = <fs_mkal>-mdv01.
     <fs_cauf>-verid = <fs_mkal>-verid.
*     para el cuadro real se buscan
*     las cantidades de la tabla AFPO
     IF i_real IS NOT INITIAL.
       <fs_cauf> = cantidades_afpo( <fs_cauf> ).
     ENDIF.
*     actualiza los datos en la tabla
     MOVE-CORRESPONDING <fs_cauf> TO ls_info.
     MOVE <fs_cauf>-plnbez TO ls_info-matnr.
     MOVE <fs_cauf>-gmein TO ls_info-meins.
*     registra la orden
     CALL METHOD append_order_info
       EXPORTING i_real = i_real
        CHANGING c_info = ls_info.
   ENDLOOP.
*   ordena la tabla resultante
   CALL METHOD sort_order_info EXPORTING i_real = i_real.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method APPEND_ORDER_INFO
*&---------------------------------------------------------------------*
 METHOD append_order_info.
*   limpia la posición del doc.
   CLEAR c_info-posnr.
*   verifica si es necesario separar la orden en 2 días
   CALL METHOD verificar_split
     EXPORTING i_real = i_real
      CHANGING c_info = c_info.
*   registra la fecha a utilizar en el ordenamiento
   CALL METHOD fecha_ordenamiento CHANGING c_info = c_info.
*   verifica que la nueva fecha esté en el rango seleccionado
   CHECK c_info-rdate IN gr_gstrs.
*   convierte el formato de nro.orden
   c_info-aufnr = format_aufnr( c_info-aufnr ).
*   verifica las unidades de medida
   CALL METHOD verificar_unidades CHANGING c_info = c_info.
*   cuadro de ordenes plan
   IF i_real IS NOT INITIAL.
     APPEND c_info TO gt_order_info_real.
   ELSE. "cuadro de ordenes reales
     APPEND c_info TO gt_order_info_plan.
   ENDIF.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method SORT_ORDER_INFO
*&---------------------------------------------------------------------*
 METHOD sort_order_info.

   DATA lv_posnr_add TYPE i.
   DATA ls_order TYPE ty_order_info.
   DATA lt_order_info TYPE STANDARD TABLE OF ty_order_info.
   FIELD-SYMBOLS <fs_order> TYPE ty_order_info.

   IF i_real IS INITIAL.
*     busca las ordenes de mantenimiento
     CALL METHOD ordenes_mantenimiento.
*     y selecciona el cuadro de ordenes plan
     lt_order_info = gt_order_info_plan.
   ELSE. "selecciona el cuadro de ordenes reales
     DELETE gt_order_info_real WHERE rdate GT sy-datum.
     lt_order_info = gt_order_info_real.
   ENDIF.

*   ordenamiento según tipo de reporte
   IF gp_sku IS NOT INITIAL.
*     ordena por: linea, material, fecha de ord., fecha y hora
     SORT lt_order_info BY mdv01 matnr rdate gstrs gsuzs aufnr.
   ELSE. "ordena por: linea, fecha ord., fecha, hora y material
     SORT lt_order_info BY mdv01 rdate gstrs gsuzs matnr aufnr.
   ENDIF.

   LOOP AT lt_order_info ASSIGNING <fs_order>.
*     si no cambia ni línea ni material ni fecha
     IF gp_sku IS NOT INITIAL AND
        <fs_order>-mdv01 = ls_order-mdv01 AND
        <fs_order>-matnr = ls_order-matnr AND
        <fs_order>-rdate = ls_order-rdate.
        <fs_order>-posnr = ls_order-posnr + 1.
     ELSEIF "para reporte sin desglose p.sku
        gp_sku IS INITIAL AND
        <fs_order>-mdv01 = ls_order-mdv01 AND
        <fs_order>-rdate = ls_order-rdate.
        <fs_order>-posnr = ls_order-posnr + 1.
     ELSE. "si cambia algo
        <fs_order>-posnr = 1.
     ENDIF.
     MOVE-CORRESPONDING <fs_order> TO ls_order.
   ENDLOOP.

*   ordenamiento según tipo de reporte
   IF gp_sku IS NOT INITIAL.
*     ordena por: linea, material, fecha de ord., fecha y hora
     SORT lt_order_info BY mdv01 matnr rdate gstrs gsuzs aufnr.
   ELSE. "ordena por: linea, fecha ord., fecha, hora y material
     SORT lt_order_info BY mdv01 rdate gstrs gsuzs matnr aufnr.
   ENDIF.

   IF i_real IS NOT INITIAL. "cuadro real
     gt_order_info_real = lt_order_info.
   ELSE. "cuadro plan
     CLEAR ls_order.
*     para el cuadro plan, las ordenes de mantenimiento deben aparecer
*     en una línea aparte de las ordenes de producción
     LOOP AT lt_order_info ASSIGNING <fs_order>.
       IF gp_sku IS NOT INITIAL AND (
          <fs_order>-mdv01 <> ls_order-mdv01 OR
          <fs_order>-matnr <> ls_order-matnr OR
          <fs_order>-rdate <> ls_order-rdate ).
           lv_posnr_add = 0.
       ELSEIF
          gp_sku IS INITIAL AND (
          <fs_order>-mdv01 <> ls_order-mdv01 OR
          <fs_order>-rdate <> ls_order-rdate ).
           lv_posnr_add = 0.
       ELSEIF "orden de mantenimiento
          <fs_order>-mdv01 IN gr_motyp.
           lv_posnr_add = lv_posnr_add + 1.
       ENDIF.
       <fs_order>-posnr = <fs_order>-posnr + lv_posnr_add.
       MOVE-CORRESPONDING <fs_order> TO ls_order.
     ENDLOOP.
     gt_order_info_plan = lt_order_info.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method PRODUCTO_TERMINADO
*&---------------------------------------------------------------------*
 METHOD producto_terminado.
* verifica si el material es un producto terminado
   IF tipo_material( i_matnr ) EQ co_producto_terminado.
     MOVE 'X' TO r.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method ORDEN_BORRADA
*&---------------------------------------------------------------------*
 METHOD orden_borrada.
   DATA lv_line TYPE bsvx-sttxt.
*  revisa el status de la orden
   CALL FUNCTION 'STATUS_TEXT_EDIT'
     EXPORTING
       objnr       = i_objnr
       only_active = 'X'
       spras       = sy-langu
     IMPORTING
       line        = lv_line.
*  la orden tiene petición de borrado
   IF lv_line CS co_peticion_borrado.
     MOVE 'X' TO r.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method TIPO_MATERIAL
*&---------------------------------------------------------------------*
 METHOD tipo_material.
   DATA wa_mara LIKE LINE OF it_mara.
* busca en la tabla interna de materiales
   READ TABLE it_mara
   INTO wa_mara
   WITH KEY matnr = i_matnr.
   IF sy-subrc = 0.
     r_mtart = wa_mara-mtart.
   ELSE.
     SELECT SINGLE *
       INTO CORRESPONDING FIELDS OF wa_mara
       FROM mara
      WHERE matnr = i_matnr.
     IF sy-subrc = 0.
       APPEND wa_mara TO it_mara.
       r_mtart = wa_mara-mtart.
     ENDIF.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method NOMBRE_MATERIAL
*&---------------------------------------------------------------------*
 METHOD nombre_material.
   DATA wa_makt LIKE LINE OF it_makt.
* busca en la tabla interna nombre de materiales
   READ TABLE it_makt
   INTO wa_makt
   WITH KEY matnr = i_matnr.
   IF sy-subrc EQ 0.
     r_maktx = wa_makt-maktx.
   ELSE.
     SELECT SINGLE maktx
       INTO r_maktx
       FROM makt
      WHERE matnr = i_matnr
        AND spras = sy-langu.
     IF sy-subrc EQ 0.
       MOVE r_maktx TO wa_makt-maktx.
       MOVE i_matnr TO wa_makt-matnr.
       APPEND wa_makt TO it_makt.
     ENDIF.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method TEXTO_LINEA
*&---------------------------------------------------------------------*
 METHOD texto_linea.
   DATA wa_crtx LIKE LINE OF it_crtx.
*   busca en la tabla interna texto de lineas
   READ TABLE it_crtx
   INTO wa_crtx
   WITH KEY arbpl = i_arbpl.
   IF sy-subrc EQ 0.
     r_ktext = wa_crtx-ktext.
   ELSE.
     SELECT SINGLE t~ktext INTO r_ktext
       FROM crhd AS c JOIN crtx AS t
       ON c~objid = t~objid
      AND c~objty = t~objty
      WHERE arbpl = i_arbpl
        AND werks = pa_werks.
     IF sy-subrc EQ 0.
       MOVE r_ktext TO wa_crtx-ktext.
       MOVE i_arbpl TO wa_crtx-arbpl.
       APPEND wa_crtx TO it_crtx.
     ENDIF.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method CANTIDADES_AFPO
*&---------------------------------------------------------------------*
 METHOD cantidades_afpo.
   FIELD-SYMBOLS: <fs_afpo> LIKE LINE OF it_afpo.
   MOVE-CORRESPONDING i_cauf TO r_cauf.
   CLEAR: r_cauf-gamng, r_cauf-wemng.
   LOOP AT it_afpo ASSIGNING <fs_afpo>
     WHERE aufnr EQ r_cauf-aufnr
       AND matnr EQ r_cauf-plnbez
       AND pwerk EQ r_cauf-werks
       AND verid EQ r_cauf-verid.
*     cantidad plan
     r_cauf-gamng =
     r_cauf-gamng + <fs_afpo>-psmng.
*     cantidad real
     r_cauf-wemng =
     r_cauf-wemng + <fs_afpo>-wemng.
*     unidad de medida
     r_cauf-gmein = <fs_afpo>-amein.
   ENDLOOP.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method FECHA_ORDENAMIENTO
*&---------------------------------------------------------------------*
 METHOD fecha_ordenamiento.
*   si la selección de turnos no es por día calendiario
*   y la fecha de entrada es menor a las 6AM,
*   la fecha real (de ordenamiento) es el día anterior
   c_info-rdate = c_info-gstrs.
   IF gp_calen IS INITIAL AND
      c_info-gsuzs < co_6am.
      c_info-rdate = c_info-rdate - 1.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method VERIFICAR_UNIDADES
*&---------------------------------------------------------------------*
 METHOD verificar_unidades.
*   si se indicó una unidad de medida distinta
   CHECK c_info-meins NE gp_meins.

   DATA lv_factor TYPE f.
*   busca el factor de conversión
   lv_factor =
   factor_conversion(
     EXPORTING
       i_matnr = c_info-matnr
       i_de = c_info-meins
       i_a = gp_meins ).
*   asigna la unidad de medida y las nuevas cantidades
   c_info-meins = gp_meins.
   c_info-gamng = lv_factor * c_info-gamng.
   c_info-wemng = lv_factor * c_info-wemng.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method VERIFICAR_SPLIT
*&---------------------------------------------------------------------*
 METHOD verificar_split.
* el split solo es necesario en el cuadro real
* cuando se indica la selección por turno calendario
 CHECK i_real IS INITIAL AND gp_calen IS NOT INITIAL.
* y la orden termina al día siguiente
 CHECK c_info-gstrs < c_info-gltrs.
* declaración de nueva orden separada
 DATA l_info LIKE c_info.
 l_info = c_info.
* obtiene las cantidades luego del split
 CALL METHOD split_cantidad
   EXPORTING
     i_ini = c_info-gsuzs
     i_fin = c_info-gluzs
   IMPORTING
     e_cant_1 = c_info-gamng
     e_cant_2 = l_info-gamng.
* primera orden es hasta las 2359
 c_info-gltrs = c_info-gstrs.
 c_info-gluzs = co_2359.
* segunda orden va desde las 12am
 l_info-gstrs = l_info-gltrs.
 l_info-gsuzs = co_12am.
* manda a empilar la primera orden
 CALL METHOD append_order_info
   EXPORTING i_real = i_real
    CHANGING c_info = c_info.
* y sigue con la segunda
 c_info = l_info.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method SPLIT_CANTIDAD
*&---------------------------------------------------------------------*
 METHOD split_cantidad.

   DATA lv_total TYPE p.
   DATA lv_tomorrow LIKE sy-datum.
   DATA lv_2359 LIKE i_fin.
   DATA lv_difference TYPE p.
   DATA lv_cantidad LIKE e_cant_1.

   lv_tomorrow = sy-datum + 1.
   lv_2359 = co_2359.
*   obtiene el tiempo total
   CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
     EXPORTING
       date1                  = sy-datum
       time1                  = i_ini
       date2                  = lv_tomorrow
       time2                  = i_fin
     IMPORTING
       timediff               = lv_total
     EXCEPTIONS
       invalid_datetime       = 1
       OTHERS                 = 2.
  IF sy-subrc <> 0.
*  Implement suitable error handling here
  ENDIF.
  CHECK lv_total <> 0.
*  obtiene el tiempo hasta las 12 am
   CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
     EXPORTING
       date1                  = sy-datum
       time1                  = i_ini
       date2                  = sy-datum
       time2                  = lv_2359
     IMPORTING
       timediff               = lv_difference
     EXCEPTIONS
       invalid_datetime       = 1
       OTHERS                 = 2.
  IF sy-subrc <> 0.
*  Implement suitable error handling here
  ENDIF.
  lv_cantidad = e_cant_1.
*  cantidad producida hasta las 12:00 am
  e_cant_1 = lv_difference * e_cant_1 / lv_total.
*  cantidad producida despues de las 12:00 am
  e_cant_2 = lv_cantidad - e_cant_1.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method NUMERO_CONTAINERS
*&---------------------------------------------------------------------*
 METHOD numero_containers.
*   determina el número de containers del reporte
   IF gi_real IS INITIAL.
      r = 1.
   ELSE.
      r = 2.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method SHOW_ALV_TREES
*&---------------------------------------------------------------------*
 METHOD show_alv_trees.
   CALL METHOD show_alv_tree
     EXPORTING i_text = text-h01
      CHANGING c_alv_ref = go_alv_plan.
   go_splitter->set_row_height( id = 1 height = 30 ).
*   verifica que tenga autorización p.cuadro REAL
   CHECK gi_real IS NOT INITIAL.
   CALL METHOD show_alv_tree
     EXPORTING i_text = text-h02
      CHANGING c_alv_ref = go_alv_real.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method SHOW_ALV_TREE
*&---------------------------------------------------------------------*
 METHOD show_alv_tree.
   DATA lo_layout TYPE REF TO cl_salv_form_layout_grid.
   CREATE OBJECT lo_layout.
   lo_layout->create_header_information( row = 1 column = 1 text = i_text ).
   c_alv_ref->set_top_of_list( lo_layout ).
   c_alv_ref->display( ).
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method SETUP_ALV_REE
*&---------------------------------------------------------------------*
 METHOD setup_alv_tree.

   DATA lv_datum TYPE datum.
   DATA lv_datetx TYPE lvc_fname.
*   funciones del alv
   DATA lo_functions TYPE REF TO cl_salv_functions_tree.
   DATA lt_func_list TYPE salv_t_ui_func.
   DATA lo_func_list LIKE LINE OF lt_func_list.
*   manejo de columnas
   DATA lo_columns TYPE REF TO cl_salv_columns_tree.
   DATA lo_column TYPE REF TO cl_salv_column_tree.
*   cabeceras de columnas: large, medium y small
   DATA lv_header_l TYPE scrtext_l.
   DATA lv_header_m TYPE scrtext_m.
   DATA lv_header_s TYPE scrtext_s.

   TRY.
    c_alv_ref->set_screen_status(
      pfstatus      =  '0100_SALV'
      report        =  sy-repid
      set_functions =  c_alv_ref->c_functions_all ).
*     oculta la función de sumatoria (no sirve)
     lo_functions = c_alv_ref->get_functions( ).
     lo_functions->set_collapse( abap_false ).
     lo_functions->set_expand( abap_false ).
     lo_functions->set_expand( abap_false ).
     lt_func_list = lo_functions->get_functions( ).
     LOOP AT lt_func_list INTO lo_func_list.
       IF lo_func_list-r_function->get_name( ) = '&CALC'.
          lo_func_list-r_function->set_visible( abap_false ).
          EXIT.
       ENDIF.
     ENDLOOP.
*     configuración de las columnas
     lo_columns = c_alv_ref->get_columns( ).
     lo_columns->set_optimize( abap_true ).
     lo_column ?= lo_columns->get_column( co_label ).
     lo_column->set_visible( abap_false ).
     lo_column ?= lo_columns->get_column( co_key_1 ).
     lo_column->set_visible( abap_false ).
     lo_column ?= lo_columns->get_column( co_key_2 ).
     lo_column->set_visible( abap_false ).
     lo_column ?= lo_columns->get_column( co_key_3 ).
     lo_column->set_visible( abap_false ).
*     columna de totales
     lo_column ?= lo_columns->get_column( co_total ).
     MOVE co_total TO: lv_header_l, lv_header_m, lv_header_s.
     lo_column->set_long_text( lv_header_l ).
     lo_column->set_medium_text( lv_header_m ).
     lo_column->set_short_text( lv_header_s ).
*     cada fecha es una columna nueva
     lv_datum = gs_gstrs-low.
     WHILE lv_datum LE gs_gstrs-high.
       MOVE lv_datum TO lv_datetx.
       lo_column ?= lo_columns->get_column( lv_datetx ).
       CONCATENATE lv_datum+6(2) lv_datum+4(2) lv_datum+0(4)
       INTO lv_header_l SEPARATED BY '/'.
       MOVE lv_header_l TO: lv_header_m, lv_header_s.
       lo_column->set_long_text( lv_header_l ).
       lo_column->set_medium_text( lv_header_m ).
       lo_column->set_short_text( lv_header_s ).
       ADD 1 TO lv_datum.
     ENDWHILE.
*     el catálogo de la tabla real no debe tener columnas a futuro
     IF i_real IS NOT INITIAL AND gs_gstrs-high > sy-datum.
       lv_datum = gs_gstrs-high.
       WHILE lv_datum > sy-datum.
         MOVE lv_datum TO lv_datetx.
         lo_column ?= lo_columns->get_column( lv_datetx ).
         lo_column->set_visible( abap_false ).
         ADD -1 TO lv_datum.
       ENDWHILE.
     ENDIF.
   CATCH cx_salv_msg cx_salv_not_found.
   ENDTRY.
*   configura los eventos
   CALL METHOD setup_alv_events
     EXPORTING i_real = i_real
      CHANGING c_alv_ref = c_alv_ref.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method SETUP_ALV_EVENTS
*&---------------------------------------------------------------------*
 METHOD setup_alv_events.
  DATA: lo_events TYPE REF TO cl_salv_events_tree.
  lo_events = c_alv_ref->get_event( ).
*  handler para tabla real
  IF i_real IS NOT INITIAL.
  SET HANDLER on_double_click_real FOR lo_events.
  ELSE. "handler para tabla plan
  SET HANDLER on_double_click_plan FOR lo_events.
  ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method ON_DOUBLE_CLICK_REAL
*&---------------------------------------------------------------------*
 METHOD on_double_click_real.
   DATA lv_subrc LIKE sy-subrc.
*   prueba primero el evento con datos de tabla plan
   CALL METHOD show_order
     EXPORTING i_real = 'X'
               i_node_key = node_key
               i_columnname = columnname.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method ON_DOUBLE_CLICK_PLAN
*&---------------------------------------------------------------------*
 METHOD on_double_click_plan.
   DATA lv_subrc LIKE sy-subrc.
*   prueba primero el evento con datos de tabla plan
   CALL METHOD show_order
     EXPORTING i_real = ' '
               i_node_key = node_key
               i_columnname = columnname.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method SHOW_ORDER
*&---------------------------------------------------------------------*
 METHOD show_order.

   DATA lr_nodes TYPE REF TO cl_salv_nodes.
   DATA lr_node  TYPE REF TO cl_salv_node.
   DATA lr_val   TYPE REF TO cl_salv_item.
   DATA lr_data  TYPE REF TO data.
*   etiqueta y orden
   FIELD-SYMBOLS <fs_label> TYPE any.
   FIELD-SYMBOLS <fs_aufnr> TYPE any.
   FIELD-SYMBOLS <fs_matnr> TYPE any.
*   información de la orden
   FIELD-SYMBOLS <fs_order> TYPE ty_order_info.

*   determina tabla plan o tabla real
   IF i_real IS NOT INITIAL.
      lr_nodes = go_alv_real->get_nodes( ).
   ELSE.
      lr_nodes = go_alv_plan->get_nodes( ).
   ENDIF.

   TRY.
     lr_node = lr_nodes->get_node( i_node_key ).
     CATCH cx_salv_msg.
   ENDTRY.
"   verifica que el nodo pertenesca al árbol
   CHECK lr_node IS NOT INITIAL.
*   obtiene la etiqueta
   lr_val =  lr_node->get_item( co_label ).
   lr_data = lr_val->get_value( ).
   ASSIGN lr_data->* TO <fs_label>.
*   verifica que sea una línea de orden
   IF <fs_label> EQ text-l00 OR <fs_label> EQ text-l01.
*      selecting field's value
      IF i_columnname IS NOT INITIAL.
        TRY.
        lr_val =  lr_node->get_item( i_columnname ).
        lr_data = lr_val->get_value( ).
        ASSIGN lr_data->* TO <fs_aufnr>.
         CATCH cx_salv_msg.
        ENDTRY.
      ENDIF.
*      verifica que la columna sea columna de datos
      CHECK <fs_aufnr> IS ASSIGNED.
*      busca el tipo de orden
      IF i_real IS NOT INITIAL.
      READ TABLE gt_order_info_real
       ASSIGNING <fs_order>
       WITH KEY aufnr = <fs_aufnr>.
      ELSE.
      READ TABLE gt_order_info_plan
       ASSIGNING <fs_order>
       WITH KEY aufnr = <fs_aufnr>.
      ENDIF.
*      consiguió una orden
      CHECK sy-subrc = 0.
*      ordenes previsionales
      IF <fs_order>-auart EQ co_orden_almacen.
        SET PARAMETER ID 'PAF' FIELD <fs_order>-aufnr.
        CALL TRANSACTION 'MD13' AND SKIP FIRST SCREEN.
      ELSEIF "ordenes de mantenimiento
        <fs_order>-auart IN gr_motyp.
        SET PARAMETER ID 'ANR' FIELD <fs_order>-aufnr.
        CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN.
      ELSE. "ordenes de producción
        SET PARAMETER ID 'ANR' FIELD <fs_order>-aufnr.
        CALL TRANSACTION 'COR3' AND SKIP FIRST SCREEN.
      ENDIF.
   ELSEIF "doble click en celda de SKU
     <fs_label> EQ text-l05.
*      selecting field's value
      IF i_columnname IS NOT INITIAL.
        TRY.
        lr_val =  lr_node->get_item( co_key_2 ).
        lr_data = lr_val->get_value( ).
        ASSIGN lr_data->* TO <fs_matnr>.
         CATCH cx_salv_msg.
        ENDTRY.
      ENDIF.
*      verifica que la columna sea columna de datos
      CHECK <fs_matnr> IS ASSIGNED.
      SET PARAMETER ID 'MAT' FIELD <fs_matnr>.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method CREATE_ALV_TREE
*&---------------------------------------------------------------------*
 METHOD create_alv_tree.
*   crea la tabla dinámica
   CALL METHOD cl_alv_table_create=>create_dynamic_table
     EXPORTING
       it_fieldcatalog = it_fieldcat
     IMPORTING
       ep_table        = c_table_ref.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method CREATE_NODES_ALV_TREES
*&---------------------------------------------------------------------*
 METHOD create_nodes_alv_trees.
*   reporte con desglose p.SKU
   IF gp_sku IS NOT INITIAL.
*   tabla plan de producción
   CALL METHOD create_nodes_alv_tree_sku
     EXPORTING i_real = ' '.
*   tabla real
   CALL METHOD create_nodes_alv_tree_sku
     EXPORTING i_real = 'X'.
   ELSE. "reporte sin desglose p.SKU
*   tabla plan de producción
   CALL METHOD create_nodes_alv_tree
     EXPORTING i_real = ' '.
*   tabla real
   CALL METHOD create_nodes_alv_tree
     EXPORTING i_real = 'X'.
   ENDIF.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method CREATE_NODES_ALV_TREE
*&---------------------------------------------------------------------*
 METHOD create_nodes_alv_tree.

   DATA lo_nodes TYPE REF TO cl_salv_nodes.
   DATA lo_node  TYPE REF TO cl_salv_node.
   DATA lk_mdv01 TYPE lvc_nkey.
   DATA lo_line  TYPE REF TO data.
   DATA lv_mdv01 TYPE mkal-mdv01.
   DATA lv_label TYPE lvc_value.
   DATA lv_count TYPE i.
   DATA lv_count_to_4 TYPE i VALUE 1.

   DATA lo_settings TYPE REF TO cl_salv_tree_settings.
   DATA lv_expand_icon TYPE salv_de_tree_image.
   DATA lv_collapse_icon TYPE salv_de_tree_image.
*   tablas dinámicas de salida del reporte
   FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.
   FIELD-SYMBOLS <fs_line> TYPE any.
   FIELD-SYMBOLS <fs_mdv01> TYPE any.
   FIELD-SYMBOLS <fs_label> TYPE any.
*   verifica que tenga autorización p.cuadro REAL
   CHECK i_real IS INITIAL OR gi_real IS NOT INITIAL.
*   Plan de producción
   IF i_real IS INITIAL.
      CALL METHOD setup_alv_tree
        EXPORTING i_real = i_real
         CHANGING c_alv_ref = go_alv_plan.
      lo_settings = go_alv_plan->get_tree_settings( ).
      lo_nodes = go_alv_plan->get_nodes( ).
      ASSIGN gt_ref_plan->* TO <fs_table>.
   ELSE. "Real
      CALL METHOD setup_alv_tree
        EXPORTING i_real = i_real
         CHANGING c_alv_ref = go_alv_real.
      lo_settings = go_alv_real->get_tree_settings( ).
      lo_nodes = go_alv_real->get_nodes( ).
      ASSIGN gt_ref_real->* TO <fs_table>.
   ENDIF.

   lv_expand_icon = icon_expand.
   lv_collapse_icon = icon_collapse.
   lo_settings->set_hierarchy_size( 30 ).
*   referencia a la instancia de los nodos del árbol
   CREATE DATA lo_line LIKE LINE OF <fs_table>.
   ASSIGN lo_line->* TO <fs_line>.
*   cantidad de registros
   DESCRIBE TABLE <fs_table> LINES lv_count.
*   llena el arbol
   LOOP AT <fs_table> ASSIGNING <fs_line>.

     ASSIGN COMPONENT co_key_1 OF STRUCTURE <fs_line> TO <fs_mdv01>.
     ASSIGN COMPONENT co_label OF STRUCTURE <fs_line> TO <fs_label>.
*     si la linea anterior fue una orden de
*     mantenimiento, omite la línea actual
     IF  lv_label = text-l00.
         lv_label = <fs_label>.
         lv_count_to_4 = lv_count_to_4 + 1.
     ELSE.
         lv_label = <fs_label>.
      IF lv_label = text-l04.
        TRY. "línea de total general
         lo_node = lo_nodes->add_node(
            related_node   = ''
            relationship   = cl_gui_column_tree=>relat_last_child
            data_row       = <fs_line>
            collapsed_icon = lv_collapse_icon
            expanded_icon  = lv_expand_icon
            row_style      = if_salv_c_tree_style=>emphasized_negative
            text           = lv_label ).
         lk_mdv01 = lo_node->get_key( ).
         CATCH cx_salv_msg.
        ENDTRY.
      ELSEIF
        lv_mdv01 <> <fs_mdv01>.
        TRY. "línea de turno
         lo_node = lo_nodes->add_node(
            related_node   = ''
            relationship   = cl_gui_column_tree=>relat_last_child
            data_row       = <fs_line>
            collapsed_icon = lv_collapse_icon
            expanded_icon  = lv_expand_icon
            row_style      = if_salv_c_tree_style=>inherited
            text           = lv_label ).
         lk_mdv01 = lo_node->get_key( ).
         IF gp_nivel > 1.
            lo_node->expand( level = 2 ).
         ENDIF.
         CATCH cx_salv_msg.
        ENDTRY.
      ELSE. "líneas: orden, SKU, cantidad, hora inicio
        lv_count_to_4 = lv_count_to_4 + 1.
        TRY.
          lo_node = lo_nodes->add_node(
             related_node   = lk_mdv01
             relationship   = cl_gui_column_tree=>relat_last_child
             data_row       = <fs_line>
             row_style      = get_alv_row_style( lv_count_to_4 )
             text           = lv_label ).
          CATCH cx_salv_msg.
        ENDTRY.
      ENDIF.
    ENDIF.
       lv_mdv01 = <fs_mdv01>.
       lv_count = lv_count - 1.
    IF lv_count = 0. EXIT. ENDIF.
   ENDLOOP.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method CREATE_NODES_ALV_TREE_SKU
*&---------------------------------------------------------------------*
 METHOD create_nodes_alv_tree_sku.

   DATA lo_nodes TYPE REF TO cl_salv_nodes.
   DATA lo_node  TYPE REF TO cl_salv_node.
   DATA lk_mdv01 TYPE lvc_nkey.
   DATA lk_matnr TYPE lvc_nkey.
   DATA lo_line  TYPE REF TO data.
   DATA lv_mdv01 TYPE mkal-mdv01.
   DATA lv_matnr TYPE mkal-matnr.
   DATA lv_label TYPE lvc_value.
   DATA lv_count TYPE i.
   DATA lv_count_to_3 TYPE i VALUE 1.

   DATA lo_settings TYPE REF TO cl_salv_tree_settings.
   DATA lv_expand_icon TYPE salv_de_tree_image.
   DATA lv_collapse_icon TYPE salv_de_tree_image.
*   tablas dinámicas de salida del reporte
   FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.
   FIELD-SYMBOLS <fs_line> TYPE any.
   FIELD-SYMBOLS <fs_mdv01> TYPE any.
   FIELD-SYMBOLS <fs_matnr> TYPE any.
   FIELD-SYMBOLS <fs_label> TYPE any.
*   verifica que tenga autorización p.cuadro REAL
   CHECK i_real IS INITIAL OR gi_real IS NOT INITIAL.
*   Plan de producción
   IF i_real IS INITIAL.
      CALL METHOD setup_alv_tree
        EXPORTING i_real = i_real
         CHANGING c_alv_ref = go_alv_plan.
      lo_settings = go_alv_plan->get_tree_settings( ).
      lo_nodes = go_alv_plan->get_nodes( ).
      ASSIGN gt_ref_plan->* TO <fs_table>.
   ELSE. "Real
      CALL METHOD setup_alv_tree
        EXPORTING i_real = i_real
         CHANGING c_alv_ref = go_alv_real.
      lo_settings = go_alv_real->get_tree_settings( ).
      lo_nodes = go_alv_real->get_nodes( ).
      ASSIGN gt_ref_real->* TO <fs_table>.
   ENDIF.

   lv_expand_icon = icon_expand.
   lv_collapse_icon = icon_collapse.
   lo_settings->set_hierarchy_size( 30 ).
*   referencia a la instancia de los nodos del árbol
   CREATE DATA lo_line LIKE LINE OF <fs_table>.
   ASSIGN lo_line->* TO <fs_line>.
*   cantidad de registros
   DESCRIBE TABLE <fs_table> LINES lv_count.
*   llena el arbol
   LOOP AT <fs_table> ASSIGNING <fs_line>.

     ASSIGN COMPONENT co_key_1 OF STRUCTURE <fs_line> TO <fs_mdv01>.
     ASSIGN COMPONENT co_key_2 OF STRUCTURE <fs_line> TO <fs_matnr>.
     ASSIGN COMPONENT co_label OF STRUCTURE <fs_line> TO <fs_label>.
*     si la linea anterior fue una orden de
*     mantenimiento, omite la línea actual
     IF  lv_label = text-l00.
         lv_label = <fs_label>.
         lv_count_to_3 = lv_count_to_3 + 1.
     ELSE.
         lv_label = <fs_label>.
      IF lv_label = text-l04.
        TRY. "línea de total general
         lo_node = lo_nodes->add_node(
            related_node   = ''
            relationship   = cl_gui_column_tree=>relat_last_child
            data_row       = <fs_line>
            collapsed_icon = lv_collapse_icon
            expanded_icon  = lv_expand_icon
            row_style      = if_salv_c_tree_style=>emphasized_negative
            text           = lv_label ).
         lk_mdv01 = lo_node->get_key( ).
         CATCH cx_salv_msg.
        ENDTRY.
      ELSEIF
        lv_mdv01 <> <fs_mdv01>.
        TRY. "línea de turno
         lo_node = lo_nodes->add_node(
            related_node   = ''
            relationship   = cl_gui_column_tree=>relat_last_child
            data_row       = <fs_line>
            collapsed_icon = lv_collapse_icon
            expanded_icon  = lv_expand_icon
            row_style      = if_salv_c_tree_style=>inherited
            text           = lv_label ).
         lk_mdv01 = lo_node->get_key( ).
         IF gp_nivel = 2.
            lo_node->expand( level = 2 ).
         ENDIF.
         CATCH cx_salv_msg.
        ENDTRY.
      ELSEIF
        lv_matnr <> <fs_matnr>.
        TRY. "línea de material
         lo_node = lo_nodes->add_node(
            related_node   = lk_mdv01
            relationship   = cl_gui_column_tree=>relat_last_child
            data_row       = <fs_line>
            collapsed_icon = lv_collapse_icon
            expanded_icon  = lv_expand_icon
            row_style      = if_salv_c_tree_style=>emphasized_a
            text           = lv_label ).
         lk_matnr = lo_node->get_key( ).
         IF gp_nivel = 3.
            lo_node->expand( level = 2 ).
         ENDIF.
         CATCH cx_salv_msg.
        ENDTRY.
      ELSE. "líneas: orden, cantidad, hora inicio
        lv_count_to_3 = lv_count_to_3 + 1.
        TRY.
          lo_node = lo_nodes->add_node(
             related_node   = lk_matnr
             relationship   = cl_gui_column_tree=>relat_last_child
             data_row       = <fs_line>
             row_style      = get_alv_row_style( lv_count_to_3 )
             text           = lv_label ).
          CATCH cx_salv_msg.
        ENDTRY.
      ENDIF.
    ENDIF.
       lv_mdv01 = <fs_mdv01>.
       lv_matnr = <fs_matnr>.
       lv_count = lv_count - 1.
    IF lv_count = 0. EXIT. ENDIF.
   ENDLOOP.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method GET_ALV_ROW_STYLE
*&---------------------------------------------------------------------*
METHOD get_alv_row_style.
 DATA lv_div TYPE i.
* si es un reporte sin desglose
* por SKU, son grupos de 3 líneas
 IF gp_sku IS NOT INITIAL.
    lv_div = 3.
 ELSE. "si no, son grupos de 4
    lv_div = 4.
 ENDIF.
* cada 3 o 4 registros modifica el color del background
* para simular un efecto zebra de separación de grupo
* de líneas: orden,[SKU],cantidad,hora de inicio.
 IF ceil( i_count / lv_div ) MOD 2 = 0.
    r_style = IF_SALV_C_TREE_STYLE=>default.
 ELSE.
    r_style = IF_SALV_C_TREE_STYLE=>emphasized_c.
 ENDIF.
ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method CREATE_FIELD_CATALOG
*&---------------------------------------------------------------------*
   METHOD create_field_catalog.
*     longitudes de los campos de la tabla
     CONSTANTS lc_label_lenght TYPE lvc_outlen VALUE 50.
     CONSTANTS lc_field_lenght TYPE lvc_outlen VALUE 15.
     CONSTANTS lc_field_wsku_l TYPE lvc_outlen VALUE 35.
     CONSTANTS lc_total_lenght TYPE lvc_outlen VALUE 15.
     CONSTANTS lc_level_lenght TYPE lvc_outlen VALUE 10.

     DATA lv_datum TYPE datum.
     DATA lv_fieldlen TYPE lvc_outlen.
     DATA wa_fieldcat TYPE lvc_s_fcat.
*     el reporte con desglose por sku
*     requiere menos espacio de celda
     IF gp_sku IS NOT INITIAL.
       lv_fieldlen = lc_field_lenght.
     ELSE. "sino, hay que poner el sku en la celda
       lv_fieldlen = lc_field_wsku_l.
     ENDIF.

*     la primera clave es la línea
     wa_fieldcat-ref_table = 'MKAL'.
     wa_fieldcat-ref_field = co_key_1.
     wa_fieldcat-fieldname = co_key_1.
     APPEND wa_fieldcat TO it_fieldcat.
*     la segunda clave es el material
     wa_fieldcat-ref_table = 'MARA'.
     wa_fieldcat-ref_field = co_key_2.
     wa_fieldcat-fieldname = co_key_2.
     APPEND wa_fieldcat TO it_fieldcat.
*     la tercera clave es la posición
     wa_fieldcat-ref_table = 'AFPO'.
     wa_fieldcat-ref_field = co_key_3.
     wa_fieldcat-fieldname = co_key_3.
     APPEND wa_fieldcat TO it_fieldcat.
     CLEAR wa_fieldcat.
*     la primera columna es una etiqueta
     wa_fieldcat-fieldname = co_label.
     wa_fieldcat-outputlen = lc_label_lenght.
     APPEND wa_fieldcat TO it_fieldcat.
     CLEAR wa_fieldcat.
     lv_datum = gs_gstrs-low.
*     cada fecha es una columna nueva
     WHILE lv_datum LE gs_gstrs-high.
       wa_fieldcat-fieldname = lv_datum.
       wa_fieldcat-outputlen = lv_fieldlen.
       APPEND wa_fieldcat TO it_fieldcat.
       ADD 1 TO lv_datum.
     ENDWHILE.
*     la última es la columna de totales
     wa_fieldcat-fieldname = co_total.
     wa_fieldcat-outputlen = lc_total_lenght.
     APPEND wa_fieldcat TO it_fieldcat.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method CREATE_DYNAMIC_TABLES
*&---------------------------------------------------------------------*
   METHOD create_dynamic_tables.
*   crea el container principal
   CREATE OBJECT go_container
    EXPORTING container_name = 'GO_CONTAINER'.
*   crea el separador
   CREATE OBJECT go_splitter
    EXPORTING
      parent   = go_container
      rows     = numero_containers( )
      columns  = 1
      align    = 15.
*   asigna el container del cuadro plan
   CALL METHOD go_splitter->get_container
     EXPORTING row = 1 column = 1
     RECEIVING container = go_container_plan.
*   crea tabla dinámica plan
   CALL METHOD create_dynamic_table
      CHANGING c_ref = gt_ref_plan
               c_alv_ref = go_alv_plan
               c_gui_ref = go_container_plan.
*   verifica que tenga autorización p.cuadro REAL
   CHECK gi_real IS NOT INITIAL.
*   asigna el container del cuadro real
   CALL METHOD go_splitter->get_container
     EXPORTING row = 2 column = 1
     RECEIVING container = go_container_real.
*   crea tabla dinámica real
   CALL METHOD create_dynamic_table
      CHANGING c_ref = gt_ref_real
               c_alv_ref = go_alv_real
               c_gui_ref = go_container_real.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
   METHOD create_dynamic_table.

   FIELD-SYMBOLS <fs_empty_table> TYPE STANDARD TABLE.
   CALL METHOD create_alv_tree CHANGING c_table_ref = c_ref.
   ASSIGN c_ref->* TO <fs_empty_table>.
*   crea el formato de la tabla de salida en el alv tree
   TRY.
       cl_salv_tree=>factory(
         EXPORTING
           r_container = c_gui_ref
         IMPORTING
           r_salv_tree = c_alv_ref
         CHANGING
           t_table     = <fs_empty_table> ).
     CATCH cx_salv_no_new_data_allowed cx_salv_error.
       EXIT.
   ENDTRY.
 ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method FILL_DYNAMIC_TABLES
*&---------------------------------------------------------------------*
   METHOD fill_dynamic_tables.
*     reporte con desglose p.SKU
     IF gp_sku IS NOT INITIAL.
*     tabla plan de producción
     CALL METHOD fill_dynamic_table_sku
       EXPORTING i_real = ' '.
*     tabla real
     CALL METHOD fill_dynamic_table_sku
       EXPORTING i_real = 'X'.
     ELSE. "sin desglose p.SKU
*     tabla plan de producción
     CALL METHOD fill_dynamic_table
       EXPORTING i_real = ' '.
*     tabla real
     CALL METHOD fill_dynamic_table
       EXPORTING i_real = 'X'.
     ENDIF.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method FILL_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
   METHOD fill_dynamic_table.

     DATA lt_info TYPE STANDARD TABLE OF ty_order_info.
     FIELD-SYMBOLS <fs_info> LIKE LINE OF lt_info.
*     campo de cantidad a utilizar: plan o real
     DATA lf_quantity TYPE lvc_s_fcat-ref_field.
     DATA lv_posnr TYPE afpo-posnr.
     DATA lv_matnr TYPE mara-matnr.
     DATA lv_mdv01 TYPE mkal-mdv01.
     DATA lv_maktx TYPE makt-maktx.
     DATA lv_ktext TYPE crtx-ktext.
     DATA lv_tabix LIKE sy-tabix.
     DATA lv_fieldname(8).

     DATA lr_table TYPE REF TO data.
     DATA ls_aufnr_line TYPE REF TO data.
     DATA ls_gamng_line TYPE REF TO data.
     DATA ls_gsuzs_line TYPE REF TO data.
     DATA ls_matnr_line TYPE REF TO data.
     DATA ls_mdv01_line TYPE REF TO data.
*     tabla dinámica de salida del reporte
     FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.
*     líneas de la tabla
     FIELD-SYMBOLS <fs_aufnr_line> TYPE any. "línea de orden
     FIELD-SYMBOLS <fs_gamng_line> TYPE any. "línea de cantidad
     FIELD-SYMBOLS <fs_gsuzs_line> TYPE any. "línea de hora
     FIELD-SYMBOLS <fs_matnr_line> TYPE any. "línea de material
     FIELD-SYMBOLS <fs_mdv01_line> TYPE any. "línea de línea
*     campos de asignación
     FIELD-SYMBOLS <fs_field> TYPE any.
     FIELD-SYMBOLS <fs_label> TYPE any.
     FIELD-SYMBOLS <fs_mdv01> TYPE mkal-mdv01.
     FIELD-SYMBOLS <fs_matnr> TYPE mara-matnr.
     FIELD-SYMBOLS <fs_posnr> TYPE afpo-posnr.
     FIELD-SYMBOLS <fs_quantity> TYPE any.
*     verifica que tenga autorización p.cuadro REAL
     CHECK i_real IS INITIAL OR gi_real IS NOT INITIAL.
*     cuadro producción real
     IF i_real IS NOT INITIAL.
       lt_info = gt_order_info_real.
       CONCATENATE '<FS_INFO>-' co_real INTO lf_quantity.
       lr_table = gt_ref_real.
     ELSE. "cuadro plan de producción
       lt_info = gt_order_info_plan.
       CONCATENATE '<FS_INFO>-' co_plan INTO lf_quantity.
       lr_table = gt_ref_plan.
     ENDIF.

     ASSIGN lr_table->* TO <fs_table>.
     CREATE DATA ls_mdv01_line LIKE LINE OF <fs_table>.
     ASSIGN ls_mdv01_line->* TO <fs_mdv01_line>.
     CREATE DATA ls_matnr_line LIKE LINE OF <fs_table>.
     ASSIGN ls_matnr_line->* TO <fs_matnr_line>.
     CREATE DATA ls_aufnr_line LIKE LINE OF <fs_table>.
     ASSIGN ls_aufnr_line->* TO <fs_aufnr_line>.
     CREATE DATA ls_gamng_line LIKE LINE OF <fs_table>.
     ASSIGN ls_gamng_line->* TO <fs_gamng_line>.
     CREATE DATA ls_gsuzs_line LIKE LINE OF <fs_table>.
     ASSIGN ls_gsuzs_line->* TO <fs_gsuzs_line>.

     LOOP AT lt_info ASSIGNING <fs_info>.
*       nueva línea de nivel 1
       IF <fs_info>-mdv01 <> lv_mdv01.
         CLEAR <fs_mdv01_line>.
         ASSIGN COMPONENT co_key_1 OF STRUCTURE <fs_mdv01_line> TO <fs_mdv01>.
         <fs_mdv01> = <fs_info>-mdv01. "línea
*         etiquetas de la líneas
         ASSIGN COMPONENT co_label OF STRUCTURE <fs_mdv01_line> TO <fs_label>.
         lv_ktext = texto_linea( <fs_info>-mdv01 ).
*         construye la etiqueta: línea - descripción
         CONCATENATE <fs_mdv01> lv_ktext INTO <fs_label> SEPARATED BY ' - '.
         INSERT <fs_mdv01_line> INTO TABLE <fs_table>.
       ENDIF.
*       nueva línea de nivel 2
       IF <fs_info>-mdv01 <> lv_mdv01 OR
          <fs_info>-matnr <> lv_matnr OR
          <fs_info>-posnr <> lv_posnr.
           MOVE-CORRESPONDING <fs_mdv01_line> TO <fs_aufnr_line>.
           ASSIGN COMPONENT co_key_3 OF STRUCTURE <fs_aufnr_line> TO <fs_posnr>.
           <fs_posnr> = <fs_info>-posnr. "posición
           ASSIGN COMPONENT co_key_2 OF STRUCTURE <fs_aufnr_line> TO <fs_matnr>.
           <fs_matnr> = format_matnr( <fs_info>-matnr ). "material
           MOVE-CORRESPONDING <fs_aufnr_line> TO:
           <fs_matnr_line>,<fs_gamng_line>,<fs_gsuzs_line>.
*           etiquetas de cada una de las líneas
           ASSIGN COMPONENT co_label OF STRUCTURE <fs_matnr_line> TO <fs_label>.
           <fs_label> = text-l05. "SKU
           ASSIGN COMPONENT co_label OF STRUCTURE <fs_aufnr_line> TO <fs_label>.
           <fs_label> = texto_auart( <fs_info>-auart ). "orden de producción/mantenimiento
           ASSIGN COMPONENT co_label OF STRUCTURE <fs_gamng_line> TO <fs_label>.
           <fs_label> = text-l02. "cantidad
           ASSIGN COMPONENT co_label OF STRUCTURE <fs_gsuzs_line> TO <fs_label>.
           <fs_label> = text-l03. "hora de inicio
*           inserta las líneas de reporte
           INSERT <fs_aufnr_line> INTO TABLE <fs_table>. "orden de prod.
           INSERT <fs_matnr_line> INTO TABLE <fs_table>. "SKU
           INSERT <fs_gamng_line> INTO TABLE <fs_table>. "cantidad
           INSERT <fs_gsuzs_line> INTO TABLE <fs_table>. "hora de inicio
       ENDIF.
*       llena las columnas a partir de la fecha del registro
       MOVE <fs_info>-rdate TO lv_fieldname.
       ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_aufnr_line> TO <fs_field>.
       <fs_field> = <fs_info>-aufnr. "orden de producción
       ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_matnr_line> TO <fs_field>.
       lv_maktx = nombre_material( <fs_info>-matnr )."material - descripción
       CONCATENATE <fs_matnr> lv_maktx INTO <fs_field> SEPARATED BY ' - '.
       ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_gamng_line> TO <fs_field>.
       ASSIGN (lf_quantity) TO <fs_quantity>. "cantidad
       <fs_field> = format_number( <fs_quantity> ).
       ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_gsuzs_line> TO <fs_field>.
       <fs_field> = time_to_string( <fs_info>-gsuzs ). "hora de inicio
       DESCRIBE TABLE <fs_table> LINES lv_tabix.
*       actualiza los valores en las columnas de fecha
       MODIFY <fs_table> FROM <fs_aufnr_line> INDEX ( lv_tabix - 3 ).
       MODIFY <fs_table> FROM <fs_matnr_line> INDEX ( lv_tabix - 2 ).
       MODIFY <fs_table> FROM <fs_gamng_line> INDEX ( lv_tabix - 1 ).
       MODIFY <fs_table> FROM <fs_gsuzs_line> INDEX ( lv_tabix ).
*       actualiza los valores de control
       lv_matnr = <fs_info>-matnr.
       lv_mdv01 = <fs_info>-mdv01.
       lv_posnr = <fs_info>-posnr.
     ENDLOOP.
*     calculo de totales
     CALL METHOD calculate_totals
        CHANGING c_table_ref = lr_table.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method FILL_DYNAMIC_TABLE_SKU
*&---------------------------------------------------------------------*
   METHOD fill_dynamic_table_sku.

     DATA lt_info TYPE STANDARD TABLE OF ty_order_info.
     FIELD-SYMBOLS <fs_info> LIKE LINE OF lt_info.
*     campo de cantidad a utilizar: plan o real
     DATA lf_quantity TYPE lvc_s_fcat-ref_field.
     DATA lv_posnr TYPE afpo-posnr.
     DATA lv_matnr TYPE mara-matnr.
     DATA lv_mdv01 TYPE mkal-mdv01.
     DATA lv_maktx TYPE makt-maktx.
     DATA lv_ktext TYPE crtx-ktext.
     DATA lv_tabix LIKE sy-tabix.
     DATA lv_fieldname(8).

     DATA lr_table TYPE REF TO data.
     DATA ls_aufnr_line TYPE REF TO data.
     DATA ls_gamng_line TYPE REF TO data.
     DATA ls_gsuzs_line TYPE REF TO data.
     DATA ls_matnr_line TYPE REF TO data.
     DATA ls_mdv01_line TYPE REF TO data.
*     tabla dinámica de salida del reporte
     FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.
*     líneas de la tabla
     FIELD-SYMBOLS <fs_aufnr_line> TYPE any. "línea de orden
     FIELD-SYMBOLS <fs_gamng_line> TYPE any. "línea de cantidad
     FIELD-SYMBOLS <fs_gsuzs_line> TYPE any. "línea de hora
     FIELD-SYMBOLS <fs_matnr_line> TYPE any. "línea de material
     FIELD-SYMBOLS <fs_mdv01_line> TYPE any. "línea de línea
*     campos de asignación
     FIELD-SYMBOLS <fs_field> TYPE any.
     FIELD-SYMBOLS <fs_label> TYPE any.
     FIELD-SYMBOLS <fs_mdv01> TYPE mkal-mdv01.
     FIELD-SYMBOLS <fs_matnr> TYPE mara-matnr.
     FIELD-SYMBOLS <fs_posnr> TYPE afpo-posnr.
     FIELD-SYMBOLS <fs_quantity> TYPE any.
*     verifica que tenga autorización p.cuadro REAL
     CHECK i_real IS INITIAL OR gi_real IS NOT INITIAL.
*     cuadro producción real
     IF i_real IS NOT INITIAL.
       lt_info = gt_order_info_real.
       CONCATENATE '<FS_INFO>-' co_real INTO lf_quantity.
       lr_table = gt_ref_real.
     ELSE. "cuadro plan de producción
       lt_info = gt_order_info_plan.
       CONCATENATE '<FS_INFO>-' co_plan INTO lf_quantity.
       lr_table = gt_ref_plan.
     ENDIF.

     ASSIGN lr_table->* TO <fs_table>.
     CREATE DATA ls_mdv01_line LIKE LINE OF <fs_table>.
     ASSIGN ls_mdv01_line->* TO <fs_mdv01_line>.
     CREATE DATA ls_matnr_line LIKE LINE OF <fs_table>.
     ASSIGN ls_matnr_line->* TO <fs_matnr_line>.
     CREATE DATA ls_aufnr_line LIKE LINE OF <fs_table>.
     ASSIGN ls_aufnr_line->* TO <fs_aufnr_line>.
     CREATE DATA ls_gamng_line LIKE LINE OF <fs_table>.
     ASSIGN ls_gamng_line->* TO <fs_gamng_line>.
     CREATE DATA ls_gsuzs_line LIKE LINE OF <fs_table>.
     ASSIGN ls_gsuzs_line->* TO <fs_gsuzs_line>.

     LOOP AT lt_info ASSIGNING <fs_info>.
*       nueva línea de nivel 1
       IF <fs_info>-mdv01 <> lv_mdv01.
         CLEAR <fs_mdv01_line>.
         ASSIGN COMPONENT co_key_1 OF STRUCTURE <fs_mdv01_line> TO <fs_mdv01>.
         <fs_mdv01> = <fs_info>-mdv01. "línea
*         etiquetas de la líneas
         ASSIGN COMPONENT co_label OF STRUCTURE <fs_mdv01_line> TO <fs_label>.
         lv_ktext = texto_linea( <fs_info>-mdv01 ).
*         construye la etiqueta: línea - descripción
         CONCATENATE <fs_mdv01> lv_ktext INTO <fs_label> SEPARATED BY ' - '.
         INSERT <fs_mdv01_line> INTO TABLE <fs_table>.
       ENDIF.
*       nueva línea de nivel 2
       IF <fs_info>-mdv01 <> lv_mdv01 OR
          <fs_info>-matnr <> lv_matnr.
         MOVE-CORRESPONDING <fs_mdv01_line> TO <fs_matnr_line>.
         ASSIGN COMPONENT co_key_2 OF STRUCTURE <fs_matnr_line> TO <fs_matnr>.
         <fs_matnr> = format_matnr( <fs_info>-matnr ). "material
*         etiquetas de la línea
         ASSIGN COMPONENT co_label OF STRUCTURE <fs_matnr_line> TO <fs_label>.
         lv_maktx = nombre_material( <fs_info>-matnr ).
*         construye la etiqueta: material - descripción
         CONCATENATE <fs_matnr> lv_maktx INTO <fs_label> SEPARATED BY ' - '.
         INSERT <fs_matnr_line> INTO TABLE <fs_table>.
       ENDIF.
*       nueva línea de nivel 3
       IF <fs_info>-mdv01 <> lv_mdv01 OR
          <fs_info>-matnr <> lv_matnr OR
          <fs_info>-posnr <> lv_posnr.
           MOVE-CORRESPONDING <fs_matnr_line> TO <fs_aufnr_line>.
           ASSIGN COMPONENT co_key_3 OF STRUCTURE <fs_aufnr_line> TO <fs_posnr>.
           <fs_posnr> = <fs_info>-posnr. "posición
           MOVE-CORRESPONDING <fs_aufnr_line> TO: <fs_gamng_line>, <fs_gsuzs_line>.
*           etiquetas de cada una de las líneas
           ASSIGN COMPONENT co_label OF STRUCTURE <fs_aufnr_line> TO <fs_label>.
           <fs_label> = texto_auart( <fs_info>-auart ). "orden de producción/mantenimiento
           ASSIGN COMPONENT co_label OF STRUCTURE <fs_gamng_line> TO <fs_label>.
           <fs_label> = text-l02. "cantidad
           ASSIGN COMPONENT co_label OF STRUCTURE <fs_gsuzs_line> TO <fs_label>.
           <fs_label> = text-l03. "hora de inicio
*           inserta las líneas de reporte
           INSERT <fs_aufnr_line> INTO TABLE <fs_table>. "orden de prod.
           INSERT <fs_gamng_line> INTO TABLE <fs_table>. "cantidad
           INSERT <fs_gsuzs_line> INTO TABLE <fs_table>. "hora de inicio
       ENDIF.
*       llena las columnas a partir de la fecha del registro
       MOVE <fs_info>-rdate TO lv_fieldname.
       ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_aufnr_line> TO <fs_field>.
       <fs_field> = <fs_info>-aufnr. "orden de producción
       ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_gamng_line> TO <fs_field>.
       ASSIGN (lf_quantity) TO <fs_quantity>. "cantidad
       <fs_field> = format_number( <fs_quantity> ).
       ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_gsuzs_line> TO <fs_field>.
       <fs_field> = time_to_string( <fs_info>-gsuzs ). "hora de inicio
       DESCRIBE TABLE <fs_table> LINES lv_tabix.
*       actualiza los valores en las columnas de fecha
       MODIFY <fs_table> FROM <fs_aufnr_line> INDEX ( lv_tabix - 2 ).
       MODIFY <fs_table> FROM <fs_gamng_line> INDEX ( lv_tabix - 1 ).
       MODIFY <fs_table> FROM <fs_gsuzs_line> INDEX ( lv_tabix ).
*       actualiza los valores de control
       lv_matnr = <fs_info>-matnr.
       lv_mdv01 = <fs_info>-mdv01.
       lv_posnr = <fs_info>-posnr.
     ENDLOOP.
*     calculo de totales
     CALL METHOD calculate_totals_sku
        CHANGING c_table_ref = lr_table.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method CALCULATE_TOTALS
*&---------------------------------------------------------------------*
METHOD calculate_totals.
* tabla dinámica de salida del reporte
 FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.
 FIELD-SYMBOLS <fs_mdv01> TYPE any.
 FIELD-SYMBOLS <fs_label> TYPE any.
 FIELD-SYMBOLS <fs_field> TYPE any.
 FIELD-SYMBOLS <fs_line>  TYPE any.
 FIELD-SYMBOLS <fs_mdv01_line> TYPE any. "línea de línea
 FIELD-SYMBOLS <fs_total_line> TYPE any. "línea de totales

 DATA lo_line TYPE REF TO data.
 DATA lo_mdv01_line TYPE REF TO data.
 DATA lo_total_line TYPE REF TO data.
* variables para el manejo de fecha
 DATA lv_datum TYPE datum.
 DATA lv_fieldname(8).
* variables para manejo de cantidades
 DATA lv_mdv01_total TYPE gamng.
 DATA lv_total TYPE gamng.
 DATA lv_decimal TYPE p.
* variables para manejo de iteración
 DATA lv_mdv01 TYPE mkal-mdv01.
 DATA lv_mdv01_tabix TYPE sy-tabix.

 ASSIGN c_table_ref->* TO <fs_table>.
 CREATE DATA lo_line LIKE LINE OF <fs_table>.
 ASSIGN lo_line->* TO <fs_line>.
 CREATE DATA lo_mdv01_line LIKE LINE OF <fs_table>.
 ASSIGN lo_mdv01_line->* TO <fs_mdv01_line>.
 CREATE DATA lo_total_line LIKE LINE OF <fs_table>.
 ASSIGN lo_total_line->* TO <fs_total_line>.

* calculo de cada celda
 lv_datum = gs_gstrs-low.
 WHILE lv_datum LE gs_gstrs-high.
  MOVE lv_datum TO lv_fieldname.
   LOOP AT <fs_table> ASSIGNING <fs_line>.
     ASSIGN COMPONENT co_key_1 OF STRUCTURE <fs_line> TO <fs_mdv01>.
     ASSIGN COMPONENT co_label OF STRUCTURE <fs_line> TO <fs_label>.
*     obtiene la línea de línea (turno)
     IF lv_mdv01 <> <fs_mdv01>.
        lv_mdv01_total = 0.
        lv_mdv01_tabix = sy-tabix.
        ASSIGN <fs_line> TO <fs_mdv01_line>.
     ELSEIF "línea de cantidad
        <fs_label> EQ text-l02.
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_line> TO <fs_field>.
        lv_decimal = string_to_number( <fs_field> ).
        lv_mdv01_total = lv_mdv01_total + lv_decimal.
        lv_total = lv_total + lv_decimal.
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_mdv01_line> TO <fs_field>.
        <fs_field> = format_number( lv_mdv01_total ).
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_total_line> TO <fs_field>.
        <fs_field> = format_number( lv_total ).
        MODIFY <fs_table> FROM <fs_mdv01_line> INDEX lv_mdv01_tabix.
     ENDIF.
     lv_mdv01 = <fs_mdv01>.
   ENDLOOP.
     lv_total = 0.
     lv_mdv01_total = 0.
     lv_datum = lv_datum + 1.
 ENDWHILE.
* agrega la línea de totales
 ASSIGN COMPONENT co_label OF STRUCTURE <fs_total_line> TO <fs_label>.
 <fs_label> = text-l04.
 INSERT <fs_total_line> INTO TABLE <fs_table>.
* calculo de totales en columna final
 LOOP AT <fs_table> ASSIGNING <fs_line>.
   ASSIGN COMPONENT co_key_1 OF STRUCTURE <fs_line> TO <fs_mdv01>.
*   obtiene la línea de línea (turno)
   IF lv_mdv01 <> <fs_mdv01>.
      lv_mdv01_total = 0.
      lv_mdv01_tabix = sy-tabix.
*      calculo de cada celda
      lv_datum = gs_gstrs-low.
      WHILE lv_datum LE gs_gstrs-high.
       MOVE lv_datum TO lv_fieldname.
       ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_line> TO <fs_field>.
       lv_decimal = string_to_number( <fs_field> ).
       lv_mdv01_total = lv_mdv01_total + lv_decimal.
       lv_datum = lv_datum + 1.
      ENDWHILE.
      ASSIGN COMPONENT co_total OF STRUCTURE <fs_line> TO <fs_field>.
      <fs_field> = format_number( lv_mdv01_total ).
      MODIFY <fs_table> FROM <fs_line> INDEX lv_mdv01_tabix.
   ENDIF.
   lv_mdv01 = <fs_mdv01>.
 ENDLOOP.
ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method CALCULATE_TOTALS_SKU
*&---------------------------------------------------------------------*
METHOD calculate_totals_sku.
* tabla dinámica de salida del reporte
 FIELD-SYMBOLS <fs_table> TYPE STANDARD TABLE.
 FIELD-SYMBOLS <fs_mdv01> TYPE any.
 FIELD-SYMBOLS <fs_matnr> TYPE any.
 FIELD-SYMBOLS <fs_label> TYPE any.
 FIELD-SYMBOLS <fs_field> TYPE any.
 FIELD-SYMBOLS <fs_line>  TYPE any.
 FIELD-SYMBOLS <fs_matnr_line> TYPE any. "línea de material
 FIELD-SYMBOLS <fs_mdv01_line> TYPE any. "línea de línea
 FIELD-SYMBOLS <fs_total_line> TYPE any. "línea de totales

 DATA lo_line TYPE REF TO data.
 DATA lo_matnr_line TYPE REF TO data.
 DATA lo_mdv01_line TYPE REF TO data.
 DATA lo_total_line TYPE REF TO data.
* variables para el manejo de fecha
 DATA lv_datum TYPE datum.
 DATA lv_fieldname(8).
* variables para manejo de cantidades
 DATA lv_mdv01_total TYPE gamng.
 DATA lv_matnr_total TYPE gamng.
 DATA lv_total TYPE gamng.
 DATA lv_decimal TYPE p.
* variables para manejo de iteración
 DATA lv_mdv01 TYPE mkal-mdv01.
 DATA lv_matnr TYPE mkal-matnr.
 DATA lv_mdv01_tabix TYPE sy-tabix.
 DATA lv_matnr_tabix TYPE sy-tabix.

 ASSIGN c_table_ref->* TO <fs_table>.
 CREATE DATA lo_line LIKE LINE OF <fs_table>.
 ASSIGN lo_line->* TO <fs_line>.
 CREATE DATA lo_mdv01_line LIKE LINE OF <fs_table>.
 ASSIGN lo_mdv01_line->* TO <fs_mdv01_line>.
 CREATE DATA lo_matnr_line LIKE LINE OF <fs_table>.
 ASSIGN lo_matnr_line->* TO <fs_matnr_line>.
 CREATE DATA lo_total_line LIKE LINE OF <fs_table>.
 ASSIGN lo_total_line->* TO <fs_total_line>.

* calculo de cada celda
 lv_datum = gs_gstrs-low.
 WHILE lv_datum LE gs_gstrs-high.
  MOVE lv_datum TO lv_fieldname.
   LOOP AT <fs_table> ASSIGNING <fs_line>.
     ASSIGN COMPONENT co_key_1 OF STRUCTURE <fs_line> TO <fs_mdv01>.
     ASSIGN COMPONENT co_key_2 OF STRUCTURE <fs_line> TO <fs_matnr>.
     ASSIGN COMPONENT co_label OF STRUCTURE <fs_line> TO <fs_label>.
*     obtiene la línea de línea (turno)
     IF lv_mdv01 <> <fs_mdv01>.
        lv_mdv01_total = 0.
        lv_mdv01_tabix = sy-tabix.
        ASSIGN <fs_line> TO <fs_mdv01_line>.
     ELSEIF "línea de material
        lv_matnr <> <fs_matnr>.
        lv_matnr_total = 0.
        lv_matnr_tabix = sy-tabix.
        ASSIGN <fs_line> TO <fs_matnr_line>.
     ELSEIF "línea de cantidad
        <fs_label> EQ text-l02.
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_line> TO <fs_field>.
        lv_decimal = string_to_number( <fs_field> ).
        lv_mdv01_total = lv_mdv01_total + lv_decimal.
        lv_matnr_total = lv_matnr_total + lv_decimal.
        lv_total = lv_total + lv_decimal.
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_mdv01_line> TO <fs_field>.
        <fs_field> = format_number( lv_mdv01_total ).
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_matnr_line> TO <fs_field>.
        <fs_field> = format_number( lv_matnr_total ).
        ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_total_line> TO <fs_field>.
        <fs_field> = format_number( lv_total ).
        MODIFY <fs_table> FROM <fs_mdv01_line> INDEX lv_mdv01_tabix.
        MODIFY <fs_table> FROM <fs_matnr_line> INDEX lv_matnr_tabix.
     ENDIF.
     lv_mdv01 = <fs_mdv01>.
     lv_matnr = <fs_matnr>.
   ENDLOOP.
     lv_total = 0.
     lv_mdv01_total = 0.
     lv_matnr_total = 0.
     lv_datum = lv_datum + 1.
 ENDWHILE.
* agrega la línea de totales
 ASSIGN COMPONENT co_label OF STRUCTURE <fs_total_line> TO <fs_label>.
 <fs_label> = text-l04.
 INSERT <fs_total_line> INTO TABLE <fs_table>.
* calculo de totales en columna final
 LOOP AT <fs_table> ASSIGNING <fs_line>.
   ASSIGN COMPONENT co_key_1 OF STRUCTURE <fs_line> TO <fs_mdv01>.
   ASSIGN COMPONENT co_key_2 OF STRUCTURE <fs_line> TO <fs_matnr>.
*   obtiene la línea de línea (turno)
   IF lv_mdv01 <> <fs_mdv01> OR
      lv_matnr <> <fs_matnr>.
      lv_mdv01_total = 0.
      lv_mdv01_tabix = sy-tabix.
*      calculo de cada celda
      lv_datum = gs_gstrs-low.
      WHILE lv_datum LE gs_gstrs-high.
       MOVE lv_datum TO lv_fieldname.
       ASSIGN COMPONENT lv_fieldname OF STRUCTURE <fs_line> TO <fs_field>.
       lv_decimal = string_to_number( <fs_field> ).
       lv_mdv01_total = lv_mdv01_total + lv_decimal.
       lv_datum = lv_datum + 1.
      ENDWHILE.
      ASSIGN COMPONENT co_total OF STRUCTURE <fs_line> TO <fs_field>.
      <fs_field> = format_number( lv_mdv01_total ).
      MODIFY <fs_table> FROM <fs_line> INDEX lv_mdv01_tabix.
   ENDIF.
   lv_mdv01 = <fs_mdv01>.
   lv_matnr = <fs_matnr>.
 ENDLOOP.
ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method TIME_TO_STRING
*&---------------------------------------------------------------------*
   METHOD time_to_string.
     CONCATENATE i_time+0(2) ':' i_time+2(2) INTO r_time.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method STRING_TO_NUMBER
*&---------------------------------------------------------------------*
   METHOD string_to_number.
*     convierte el valor a número
     CALL FUNCTION 'HRCM_STRING_TO_AMOUNT_CONVERT'
       EXPORTING
         string              = i_string
         decimal_separator   = ','
         thousands_separator = '.'
       IMPORTING
         betrg               = r_value
       EXCEPTIONS
         convert_error       = 1
         OTHERS              = 2.
     IF sy-subrc <> 0.
*     Implement suitable error handling here
     ENDIF.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method FORMAT_NUMBER
*&---------------------------------------------------------------------*
   METHOD format_number.
     DATA l_string(15).
     WRITE i_value TO l_string UNIT 'C12'.
     MOVE l_string TO r_string.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method TEXTO_AUART
*&---------------------------------------------------------------------*
   METHOD texto_auart.
     "orden de mantenimiento
     IF i_auart IN gr_motyp.
        r_text = text-l00.
     ELSE. "orden de producción
        r_text = text-l01.
     ENDIF.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method TEXTO_MEINS
*&---------------------------------------------------------------------*
   METHOD texto_meins.
*     obtiene el texto de la U/M
     SELECT SINGLE mseht INTO r_text
       FROM t006a
      WHERE spras EQ 'S'
        AND msehi EQ i_meins.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method FORMAT_AUFNR
*&---------------------------------------------------------------------*
   METHOD format_aufnr.
     CALL FUNCTION 'CONVERSION_EXIT_AUFNR_OUTPUT'
       EXPORTING
         input  = i_aufnr
       IMPORTING
         output = r_aufnr.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method FORMAT_MATNR
*&---------------------------------------------------------------------*
   METHOD format_matnr.
     r_matnr = i_matnr.
     SHIFT r_matnr LEFT DELETING LEADING '0'.
   ENDMETHOD.
*&---------------------------------------------------------------------*
*& Private Method FACTOR_CONVERSION
*&---------------------------------------------------------------------*
   METHOD factor_conversion.

     DATA wa_log LIKE LINE OF gt_log.
     DATA lv_maktx TYPE makt-maktx.
     DATA lv_matnr TYPE mara_matnr.

     CHECK i_matnr IS NOT INITIAL AND
     i_matnr NE text-t01. "SIN MATERIAL
     FIELD-SYMBOLS <fs_fact> TYPE ty_fact.
     READ TABLE it_fact
     ASSIGNING <fs_fact>
     WITH KEY
     matnr = i_matnr
     nach  = i_a
     von   = i_de.
*     si ya tiene el factor de conversión
     IF <fs_fact> IS ASSIGNED.
          r_fact = <fs_fact>-factor.
     ELSE." obtiene factor de conversión
       CALL FUNCTION 'MC_UNIT_CONVERSION'
         EXPORTING
           matnr      = i_matnr
           nach_meins = i_a
           von_meins  = i_de
         IMPORTING
           umref      = r_fact
         EXCEPTIONS
           conversion_not_found = 1
           material_not_found   = 2
           nach_meins_messing   = 3
           overflow             = 4
           von_meins_missing    = 5
           OTHERS               = 6.
       IF sy-subrc <> 0.
*          si no consigue factor de conversión
          lv_maktx = nombre_material( i_matnr ).
          lv_matnr = format_matnr( i_matnr ).
          CONCATENATE lv_matnr '-' lv_maktx
          INTO wa_log-msgv3 SEPARATED BY space.
          wa_log-msgid = 'ZMM'.
          wa_log-msgty = sy-msgty.
          wa_log-msgno = '001'.
          wa_log-msgv1 = texto_meins( i_de ).
          wa_log-msgv2 = texto_meins( i_a ).
          APPEND wa_log TO gt_log.
       ENDIF.
     ENDIF.
   ENDMETHOD.
 ENDCLASS. 

*----------------------------------------------------------------------*
***INCLUDE ZPPO_PROGR_PRODUCCION_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FO_VALIDAR_FECHAS
*&---------------------------------------------------------------------*
FORM fo_validar_fechas.
 "El período de selección debe ser menor de 31 días"
 IF so_gstrs-high - so_gstrs-low > pa_maxds.
   MESSAGE e398(00) WITH text-m01  pa_maxds text-m02.
 ENDIF.
ENDFORM.                    " FO_VALIDAR_FECHAS
*&---------------------------------------------------------------------*
*&      Form  FO_VALIDAR_NIVELES
*&---------------------------------------------------------------------*
FORM fo_validar_niveles.
 "El número de niveles debe ser de 1 a 3"
 IF NOT pa_nivel BETWEEN 1 AND 3.
   MESSAGE e398(00) WITH text-m03.
 ENDIF.
ENDFORM.                    " FO_VALIDAR_NIVELES
*&---------------------------------------------------------------------*
*&      Form  FO_INICIALIZAR_WERKS
*&---------------------------------------------------------------------*
FORM fo_inicializar_werks.
   DATA wa_werks LIKE LINE OF so_werks.
*   rango de centros
   wa_werks-sign = 'I'.
   wa_werks-option = 'EQ'.
   wa_werks-low = pa_werks.
   INSERT wa_werks INTO TABLE so_werks.
ENDFORM.                    " FO_INICIALIZAR_WERKS
*&---------------------------------------------------------------------*
*&      Form  FO_SELECCIONAR_DATOS
*&---------------------------------------------------------------------*
FORM fo_seleccionar_datos.
* inicialización de datos
 PERFORM fo_inicializar_werks.
* creación del reporte
 CREATE OBJECT go_reporte
   EXPORTING
     i_werks = so_werks[]
     i_gstrs = so_gstrs[]
     i_mdv01 = so_mdv01[]
     i_matnr = so_matnr[]
     i_dispo = so_dispo[]
     i_cotyp = so_cotyp[]
     i_motyp = so_motyp[]
     i_potyp = so_potyp[]
     i_cousr = so_cousr[]
     i_mousr = so_mousr[]
     i_pousr = so_pousr[]
     i_meins = pa_meins
     i_calen = pa_calen
     i_nivel = pa_nivel
     i_sku   = rb_matnr.

 CALL METHOD go_reporte->validar_autorizaciones.
 CALL METHOD go_reporte->seleccionar_datos.
* verificación de datos seleccionados
 IF go_reporte->gt_order_info_real IS INITIAL AND
    go_reporte->gt_order_info_plan IS INITIAL.
    MESSAGE i398(00) WITH
    'No se encontraron datos para'
    'el criterio especificado.' '' ''.
    LEAVE LIST-PROCESSING.
 ELSEIF "si ocurrieron errores en la selección
     go_reporte->gt_log IS NOT INITIAL.
     PERFORM fo_mostrar_log.
     LEAVE LIST-PROCESSING.
 ENDIF.
ENDFORM.                    " FO_SELECCIONAR_DATOS
*&---------------------------------------------------------------------*
*&      Form  FO_MOSTRAR_SELECCION
*&---------------------------------------------------------------------*
FORM fo_mostrar_seleccion.
 CALL SCREEN 100.
ENDFORM.                    " FO_MOSTRAR_SELECCION
*&---------------------------------------------------------------------*
*&      Form  FO_IMPRESION_REPORTE
*&---------------------------------------------------------------------*
FORM fo_impresion_reporte.
 CALL METHOD go_reporte->impresion_reporte.
ENDFORM.                    " FO_IMPRESION_REPORTE
*&---------------------------------------------------------------------*
*&      Form  FO_MOSTRAR_LOG
*&---------------------------------------------------------------------*
FORM fo_mostrar_log.
 SORT go_reporte->gt_log.
 DELETE ADJACENT DUPLICATES FROM go_reporte->gt_log
 COMPARING ALL FIELDS.
 CALL FUNCTION 'UPC_LOG_SHOW_POPUP'
   EXPORTING it_mesg = go_reporte->gt_log.
ENDFORM.                    " FO_MOSTRAR_LOG 

*----------------------------------------------------------------------*
***INCLUDE ZPPO_PROGR_PRODUCCION_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.
 CASE ok_code.
   WHEN 'CANCEL'.
     SET SCREEN 0.
     LEAVE SCREEN.
   WHEN 'BACK'.
     SET SCREEN 0.
     LEAVE SCREEN.
   WHEN 'EXIT'.
     SET SCREEN 0.
     LEAVE SCREEN.
 ENDCASE.
ENDMODULE.                 " PAI_0100  INPUT 

*----------------------------------------------------------------------*
***INCLUDE ZPPO_PROGR_PRODUCCION_PBO.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
 SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
 PERFORM fo_impresion_reporte.
ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0100_HEADER  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0100_header OUTPUT.

 CONSTANTS lv_to VALUE 'a'.
 DATA lv_date(10).
 DATA lv_meins TYPE t006a-mseht.
 CLEAR SCREEN_HEADER.
* obtiene el texto de la U/M
 SELECT SINGLE mseht INTO lv_meins
   FROM t006a
  WHERE spras EQ 'S'
    AND msehi EQ pa_meins.
* Centro
 IF pa_werks IS NOT INITIAL.
    CONCATENATE 'Centro:' pa_werks space
    INTO SCREEN_HEADER SEPARATED BY SPACE.
 ENDIF. "periodo desde
 IF so_gstrs-low IS NOT INITIAL.
    CONCATENATE so_gstrs-low+6(2) so_gstrs-low+4(2) so_gstrs-low+0(4)
    INTO lv_date SEPARATED BY '/'.
    CONCATENATE SCREEN_HEADER 'Período:' lv_date space
    INTO SCREEN_HEADER SEPARATED BY SPACE.
 ENDIF. "hasta
 IF so_gstrs-high IS NOT INITIAL.
    CONCATENATE so_gstrs-high+6(2) so_gstrs-high+4(2) so_gstrs-high+0(4)
    INTO lv_date SEPARATED BY '/'.
    CONCATENATE SCREEN_HEADER lv_to lv_date space
    INTO SCREEN_HEADER SEPARATED BY SPACE.
 ENDIF. "línea
 IF so_mdv01-low IS NOT INITIAL.
    CONCATENATE SCREEN_HEADER 'Línea:' so_mdv01-low space
    INTO SCREEN_HEADER SEPARATED BY SPACE.
 ENDIF.
 IF so_mdv01-high IS NOT INITIAL.
    CONCATENATE SCREEN_HEADER lv_to so_mdv01-high space
    INTO SCREEN_HEADER SEPARATED BY SPACE.
 ENDIF. "material
 IF so_matnr-low IS NOT INITIAL.
    CONCATENATE SCREEN_HEADER 'Material:' so_matnr-low space
    INTO SCREEN_HEADER SEPARATED BY SPACE.
 ENDIF.
 IF so_matnr-high IS NOT INITIAL.
    CONCATENATE SCREEN_HEADER lv_to so_matnr-high space
    INTO SCREEN_HEADER SEPARATED BY SPACE.
 ENDIF. "planificador
 IF so_dispo-low IS NOT INITIAL.
    CONCATENATE SCREEN_HEADER 'Planificador:' so_dispo-low space
    INTO SCREEN_HEADER SEPARATED BY SPACE.
 ENDIF.
 IF so_dispo-high IS NOT INITIAL.
    CONCATENATE SCREEN_HEADER lv_to so_dispo-high space
    INTO SCREEN_HEADER SEPARATED BY SPACE.
 ENDIF. "Unidad de medida
 IF pa_meins IS NOT INITIAL.
    CONCATENATE SCREEN_HEADER 'U/M:' lv_meins
    INTO SCREEN_HEADER SEPARATED BY SPACE.
 ENDIF.
ENDMODULE.                 " PBO_0100_HEADER  OUTPUT 
