REPORT zrstxpdft4 LINE-SIZE 80.
*
* Read spool job contents (OTF or ABAP list) and convert
* to PDF, download PDF
* B20K8A0IKH replace WS_DOWNLOAD with GUI_DOWNLOAD
*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-010.
PARAMETERS:
  spoolno LIKE tsp01-rqident,
  download AS CHECKBOX DEFAULT 'X',
  p_file LIKE rlgrap-filename DEFAULT 'C:\temp\file.pdf'.   "#EC NOTEXT
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
     SELECTION-SCREEN PUSHBUTTON (10) but1 USER-COMMAND cli1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.
*
DATA otf LIKE itcoo OCCURS 100 WITH HEADER LINE.
DATA cancel.
DATA pdf LIKE tline OCCURS 100 WITH HEADER LINE.
DATA doctab LIKE docs OCCURS 1 WITH HEADER LINE.
DATA: numbytes TYPE i,
      arc_idx LIKE toa_dara,
      pdfspoolid LIKE tsp01-rqident,
      jobname LIKE tbtcjob-jobname,
      jobcount LIKE tbtcjob-jobcount,
      is_otf.
DATA: client LIKE tst01-dclient,
      name LIKE tst01-dname,
      objtype LIKE rststype-type,
      type LIKE rststype-type.
TABLES: tsp01, sscrfields.
INITIALIZATION.
  but1 = 'Ver Spool'.
AT SELECTION-SCREEN.
  IF sscrfields EQ 'CLI1'.
    CALL TRANSACTION 'SP02'.
  ENDIF.
START-OF-SELECTION.
  but1 = 'Ver Spool'.
  SELECT SINGLE * FROM tsp01 WHERE rqident = spoolno.
  IF sy-subrc <> 0.
    PERFORM bd_textbox_err(rstxpdft) USING 80
     'Spoolauftrag existiert nicht'(003).
    EXIT.
  ENDIF.
  client = tsp01-rqclient.
  name   = tsp01-rqo1name.
  CALL FUNCTION 'RSTS_GET_ATTRIBUTES'
         EXPORTING
              authority     = 'SP01'
              client        = client
              name          = name
              part          = 1
         IMPORTING
*           CHARCO        =
*           CREATER       =
*           CREDATE       =
*           DELDATE       =
*           MAX_CREDATE   =
*           MAX_DELDATE   =
*           NON_UNIQ      =
*           NOOF_PARTS    =
*           RECTYP        =
*           SIZE          =
*           STOTYP        =
              type          = type
              objtype       = objtype
         EXCEPTIONS
              fb_error      = 1
              fb_rsts_other = 2
              no_object     = 3
              no_permission = 4.
  IF objtype(3) = 'OTF'.
    is_otf = 'X'.
  ELSE.
    is_otf = space.
  ENDIF.
  IF is_otf = 'X'.
    CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid                    = spoolno
          no_dialog                      = ' '
*       DST_DEVICE                     =
*       PDF_DESTINATION                =
        IMPORTING
          pdf_bytecount                  = numbytes
          pdf_spoolid                    = pdfspoolid
*       OTF_PAGECOUNT                  =
          btc_jobname                    = jobname
          btc_jobcount                   = jobcount
        TABLES
          pdf                            = pdf
        EXCEPTIONS
          err_no_otf_spooljob            = 1
          err_no_spooljob                = 2
          err_no_permission              = 3
          err_conv_not_possible          = 4
          err_bad_dstdevice              = 5
          user_cancelled                 = 6
          err_spoolerror                 = 7
          err_temseerror                 = 8
          err_btcjob_open_failed         = 9
          err_btcjob_submit_failed       = 10
          err_btcjob_close_failed        = 11.
    CASE sy-subrc.
      WHEN 0.
        PERFORM bd_textbox_msg(rstxpdft) USING 80
         'Funktion CONVERT_OTFSPOOLJOB_2_PDF erfolgreich'(001).
      WHEN 1.
        PERFORM bd_textbox_err(rstxpdft) USING 80
         'Kein OTF- und kein ABAP-Spoolauftrag'(002).
        EXIT.
      WHEN 2.
        PERFORM bd_textbox_err(rstxpdft) USING 80
         'Spoolauftrag existiert nicht'(003).
        EXIT.
      WHEN 3.
        PERFORM bd_textbox_err(rstxpdft) USING 80
         'Keine Berechtigung zum Lesen Spoolauftrag'(004).
        EXIT.
      WHEN OTHERS.
        PERFORM bd_textbox_err(rstxpdft) USING 80
         'Fehler bei Funktion CONVERT_OTFSPOOLJOB_2_PDF'(005).
        EXIT.
    ENDCASE.
  ELSE.
    CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid                    = spoolno
          no_dialog                      = ' '
*       DST_DEVICE                     =
*       PDF_DESTINATION                =
        IMPORTING
          pdf_bytecount                  = numbytes
          pdf_spoolid                    = pdfspoolid
*       LIST_PAGECOUNT                 =
          btc_jobname                    = jobname
          btc_jobcount                   = jobcount
        TABLES
          pdf                            = pdf
        EXCEPTIONS
          err_no_abap_spooljob           = 1
          err_no_spooljob                = 2
          err_no_permission              = 3
          err_conv_not_possible          = 4
          err_bad_destdevice             = 5
          user_cancelled                 = 6
          err_spoolerror                 = 7
          err_temseerror                 = 8
          err_btcjob_open_failed         = 9
          err_btcjob_submit_failed       = 10
          err_btcjob_close_failed        = 11.
    CASE sy-subrc.
      WHEN 0.
        PERFORM bd_textbox_msg(rstxpdft) USING 80
         'Funktion CONVERT_ABAPSPOOLJOB_2_PDF erfolgreich'(006).
      WHEN 1.
        PERFORM bd_textbox_err(rstxpdft) USING 80
         'Kein OTF- und kein ABAP-Spoolauftrag'(002).
        EXIT.
      WHEN 2.
        PERFORM bd_textbox_err(rstxpdft) USING 80
         'Spoolauftrag existiert nicht'(003).
        EXIT.
      WHEN 3.
        PERFORM bd_textbox_err(rstxpdft) USING 80
         'Keine Berechtigung zum Lesen Spoolauftrag'(004).
        EXIT.
      WHEN OTHERS.
        PERFORM bd_textbox_err(rstxpdft) USING 80
         'Fehler bei Funktion CONVERT_ABAPSPOOLJOB_2_PDF'(007).
        EXIT.
    ENDCASE.
  ENDIF.
*************** download PDF file ***********
  CHECK download = 'X'.
  IF NOT ( jobname IS INITIAL ).
    PERFORM bd_textbox_var2_msg(rstxpdft) USING 80
     'Konvertierung per Hintergrundjob'(008)
     jobname
     jobcount.
    EXIT.
  ENDIF.
  PERFORM download_w_ext(rstxpdft) TABLES pdf
                                   USING p_file
                                         '.pdf'
                                         'BIN'
                                         numbytes
                                         cancel.
  IF cancel = space.
    DATA: s(80).
    s = numbytes. CONDENSE s.
    CONCATENATE s 'Bytes heruntergeladen in Datei'(009)
      INTO s SEPARATED BY space.
    PERFORM bd_textbox_var1_msg(rstxpdft) USING 80
                                      s
                                      p_file.
  ENDIF. 
