class zcl_rt_prc_cond definition
  public
  create private .

  public section.

    types:
      begin of ty_s_pricing_data,
        matnr     type matnr,
        pltyp     type pltyp,           "pricelist type (e.g 'XN')
        datab     type datab,
        datbi     type datbi,
        kbetr     type kbetr,
        meins     type meins,
        konwa     type konwa,           "currency (e.g 'PLN')
        kpein     type kpein,           "Condition pricing unit for KONP (e.g 1)
        zaehk_ind type e1konp-zaehk_ind,   "Cond.Item Index for KONP (e.g '1')
      end of ty_s_pricing_data .
    types:
      ty_t_pricing_data type table of ty_s_pricing_data .

    types: begin of ty_s_idoc_conf,
             direct type edi_direct,
             rcvprn type edi_rcvprn,
             sndpor type edi_sndpor,
             sndprt type edi_sndprt,
             mestyp type edi_mestyp,
             idoctp type edi_idoctp,
             mesfct type edi_mesfct,
             rcvpor type edi_rcvpor,
             rcvprt type edi_rcvprt,
             sndprn type sndprn,
           end of  ty_s_idoc_conf.

    types: begin of ty_s_prc_conf,
             vkorg   type vkorg,      "sales org. (e.g DS02)
             vtweg   type vtweg,      " distribution channel (e.g D3)
             kvewe   type kvewe,      "usage ( 'A' for pricing)
             kappl   type kappl,      "application ('V' for sales)
             kschl   type kschl,      "condition type (e.g VKP0 for A155)
             kotabnr type kotabnr,    "table number (e.g 155)
             stfkz   type stfkz,      "scale type (e.g. 'A')
             krech   type krech,      "calculation type (e.g. 'C')
           end of ty_s_prc_conf.
    types: ty_t_edidc type table of edidc.
    types: ty_t_edidd type table of edidd.

    class-data:
      gt_idoc_conf type table of  edidc,
      gt_idoc_data type table of  edidd.


    class-methods:
      create_prc_condition
        importing
          !it_pricing_data type ty_t_pricing_data
          !is_idoc_conf    type ty_s_idoc_conf
          !is_prc_conf     type ty_s_prc_conf,

      set_config
        importing
          !is_idoc_conf type ty_s_idoc_conf
          !is_prc_conf  type ty_s_prc_conf
        changing
          ct_config     type ty_t_edidc
        .

  protected section.

  private section.

    data:
      mt_pricing_data type ty_t_pricing_data,
      ms_e1komg       type e1komg,
      mt_e1komg       type table of e1komg.

    methods:
      constructor,
      update_with_base_uom
        changing
          ct_pricing_data type ty_t_pricing_data,

      uom_to_iso
        changing
          !cv_meins type meins,

      update_e1komg
        importing
          !is_pricing_data type ty_s_pricing_data
          !is_idoc_conf    type ty_s_idoc_conf
          !is_prc_conf     type ty_s_prc_conf
        changing
          !ct_idoc_data    type ty_t_edidd,


      update_e1konh
        importing
          !is_pricing_data type ty_s_pricing_data
        changing
          ct_idoc_data     type ty_t_edidd,

      update_e1konp
        importing
          !is_prc_conf     type  ty_s_prc_conf
          !is_pricing_data type ty_s_pricing_data
        changing
          ct_idoc_data     type ty_t_edidd,

      build_idoc_segments
        importing
          !is_pricing_data type ty_s_pricing_data
          !is_idoc_conf    type ty_s_idoc_conf
          !is_prc_conf     type ty_s_prc_conf
        changing
          !ct_idoc_data    type ty_t_edidd,

      send_idoc.

ENDCLASS.



CLASS ZCL_RT_PRC_COND IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_RT_PRC_COND=>CREATE_PRC_CONDITION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_PRICING_DATA                TYPE        TY_T_PRICING_DATA
* | [--->] IS_IDOC_CONF                   TYPE        TY_S_IDOC_CONF
* | [--->] IS_PRC_CONF                    TYPE        TY_S_PRC_CONF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method   create_prc_condition.

    set_config(
      exporting
      is_idoc_conf = is_idoc_conf
      is_prc_conf = is_prc_conf
      changing
        ct_config = gt_idoc_conf ).

    loop at it_pricing_data assigning field-symbol(<fs_pricing_data>). " przeniesc petle wyzej, poza metode

      data: mr_obj type ref to zcl_rt_prc_cond.
      mr_obj = new zcl_rt_prc_cond( ).

      mr_obj->build_idoc_segments(
         exporting
           !is_pricing_data = <fs_pricing_data>
           !is_idoc_conf = is_idoc_conf
           !is_prc_conf = is_prc_conf
         changing
           ct_idoc_data = gt_idoc_data
           ).

      mr_obj->send_idoc( ).

    endloop.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_RT_PRC_COND=>SET_CONFIG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_IDOC_CONF                   TYPE        TY_S_IDOC_CONF
* | [--->] IS_PRC_CONF                    TYPE        TY_S_PRC_CONF
* | [<-->] CT_CONFIG                      TYPE        TY_T_EDIDC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method set_config.

    data: ls_conf type edidc.

    move-corresponding is_idoc_conf to ls_conf.
    move-corresponding is_prc_conf to ls_conf.

    ls_conf-rcvpor = |SAP{ sy-sysid }|.
    ls_conf-rcvprn = |{ sy-sysid }MNDT{ sy-mandt }|.
    ls_conf-sndprn = ls_conf-rcvprn.
    append ls_conf to ct_config.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_RT_PRC_COND->UPDATE_WITH_BASE_UOM
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_PRICING_DATA                TYPE        TY_T_PRICING_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method update_with_base_uom.

    data: lt_matnr type table of matnr.

    loop at ct_pricing_data assigning field-symbol(<fs_prc_data>).
      append <fs_prc_data>-matnr to lt_matnr.
    endloop.
    sort lt_matnr.
    delete adjacent duplicates from lt_matnr.

    if lt_matnr is initial.
      return.
    endif.

    select meins, matnr from mara
      into table @data(lt_meins)
      for all entries in @lt_matnr
      where matnr = @lt_matnr-table_line.

    loop at ct_pricing_data assigning <fs_prc_data>.
      read table lt_meins into data(ls_meins) with key matnr = <fs_prc_data>-matnr.
      if sy-subrc is not initial.
        continue.
      endif.
      <fs_prc_data>-meins = ls_meins-meins.

      call method uom_to_iso( changing cv_meins = ls_meins-meins ).

    endloop.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_RT_PRC_COND->BUILD_IDOC_SEGMENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_PRICING_DATA                TYPE        TY_S_PRICING_DATA
* | [--->] IS_IDOC_CONF                   TYPE        TY_S_IDOC_CONF
* | [--->] IS_PRC_CONF                    TYPE        TY_S_PRC_CONF
* | [<-->] CT_IDOC_DATA                   TYPE        TY_T_EDIDD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method build_idoc_segments.

    data:
      ls_e1konh type e1konh,
      ls_e1konp type e1konp.

    update_e1komg(
       exporting
            is_pricing_data = is_pricing_data
            is_idoc_conf = is_idoc_conf
            is_prc_conf = is_prc_conf
       changing
           ct_idoc_data = gt_idoc_data ).

    update_e1konh(
       exporting
         is_pricing_data = is_pricing_data
      changing
         ct_idoc_data = gt_idoc_data ).

    update_e1konp(
       exporting
         is_prc_conf = is_prc_conf
         is_pricing_data = is_pricing_data
      changing
         ct_idoc_data = gt_idoc_data ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_RT_PRC_COND->SEND_IDOC
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method send_idoc.
    data: lt_idoc_data          type table of edidd,
          lt_idoc_status        type table of bdidocstat,
          lt_return_variables   type table of bdidocstat,
          lt_serializaiton_info type table of bdi_ser,
          lv_input_method       type  bdwfap_par-inputmethd,
          lv_mass_processing    type bdwfap_par-mass_proc.

    call function 'IDOC_INPUT_COND_A'
      exporting
        input_method          = lv_input_method
        mass_processing       = lv_mass_processing
*         IMPORTING
*       WORKFLOW_RESULT       =
*       APPLICATION_VARIABLE  =
*       IN_UPDATE_TASK        =
*       CALL_TRANSACTION_DONE =
      tables
        idoc_contrl           = gt_idoc_conf
        idoc_data             = gt_idoc_data
        idoc_status           = lt_idoc_status
        return_variables      = lt_return_variables
        serialization_info    = lt_serializaiton_info
      exceptions
        wrong_function_called = 1
        others                = 2.

    if sy-subrc <> 0.
      return.
    endif.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_RT_PRC_COND->UOM_TO_ISO
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CV_MEINS                       TYPE        MEINS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method uom_to_iso.

    call function 'UNIT_OF_MEASURE_SAP_TO_ISO'
      exporting
        sap_code    = cv_meins
      importing
        iso_code    = cv_meins
      exceptions
        not_found   = 1
        no_iso_code = 2
        others      = 3.
    if sy-subrc <> 0.
      return.
    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_RT_PRC_COND->UPDATE_E1KOMG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_PRICING_DATA                TYPE        TY_S_PRICING_DATA
* | [--->] IS_IDOC_CONF                   TYPE        TY_S_IDOC_CONF
* | [--->] IS_PRC_CONF                    TYPE        TY_S_PRC_CONF
* | [<-->] CT_IDOC_DATA                   TYPE        TY_T_EDIDD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method update_e1komg.

    data: ls_idoc_data type edidd.

    move-corresponding is_prc_conf to ms_e1komg.
    move-corresponding is_idoc_conf to ms_e1komg.

    ms_e1komg-vrkme = is_pricing_data-meins.
    ms_e1komg-matnr_long = is_pricing_data-matnr.

    clear ms_e1komg-vakey.
    concatenate
      is_prc_conf-vkorg
      is_prc_conf-vtweg
      is_pricing_data-pltyp
      is_pricing_data-matnr
      is_pricing_data-meins
    into ms_e1komg-vakey.

    move ms_e1komg to ls_idoc_data-sdata.
    ls_idoc_data-segnam = 'E1KOMG'.
    ls_idoc_data-hlevel = '01'.
    ls_idoc_data-segnum = 1.
    append ls_idoc_data to gt_idoc_data.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_RT_PRC_COND->UPDATE_E1KONH
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_PRICING_DATA                TYPE        TY_S_PRICING_DATA
* | [<-->] CT_IDOC_DATA                   TYPE        TY_T_EDIDD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method update_e1konh.
    data: ls_e1konh    type e1konh,
          ls_idoc_data type edidd.

    ls_e1konh-datab = is_pricing_data-datab.
    ls_e1konh-datbi = is_pricing_data-datbi.
    move ls_e1konh to ls_idoc_data-sdata.
    ls_idoc_data-segnam = 'E1KONH'.
    ls_idoc_data-hlevel = '02'.
    ls_idoc_data-psgnum = 1.
    ls_idoc_data-segnum = 2.
    append ls_idoc_data to gt_idoc_data.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_RT_PRC_COND->UPDATE_E1KONP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_PRC_CONF                    TYPE        TY_S_PRC_CONF
* | [--->] IS_PRICING_DATA                TYPE        TY_S_PRICING_DATA
* | [<-->] CT_IDOC_DATA                   TYPE        TY_T_EDIDD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method update_e1konp.
    data: ls_e1konp    type e1konp,
          ls_idoc_data type edidd.

    move-corresponding is_prc_conf to ls_e1konp.
    move-corresponding is_pricing_data to ls_e1konp.
    ls_e1konp-kmein = is_pricing_data-meins.

    move ls_e1konp to ls_idoc_data-sdata.
    ls_idoc_data-segnam = 'E1KONP'.
    ls_idoc_data-hlevel = '03'.
    ls_idoc_data-psgnum = 2.
    ls_idoc_data-segnum = 3.
    append ls_idoc_data  to gt_idoc_data.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_RT_PRC_COND->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.
  endmethod.
ENDCLASS.
