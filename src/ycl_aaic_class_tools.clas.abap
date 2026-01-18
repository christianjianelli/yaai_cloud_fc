CLASS ycl_aaic_class_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_description       TYPE yde_aaic_fc_description
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_package           TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response)   TYPE string.

    METHODS add_method
      IMPORTING
                i_class_name         TYPE yde_aaic_class_name
                i_method_name        TYPE yde_aaic_method_name
                i_description        TYPE yde_aaic_fc_description
                i_transport_request  TYPE yde_aaic_fc_transport_request
                i_package            TYPE yde_aaic_fc_package
                i_source             TYPE string
                i_s_returning_params TYPE yst_aaic_fc_method_return_par OPTIONAL
                i_t_importing_params TYPE ytt_aaic_fc_method_imp_par OPTIONAL
                i_t_exporting_params TYPE ytt_aaic_fc_method_exp_par OPTIONAL
                i_t_changing_params  TYPE ytt_aaic_fc_method_chang_par OPTIONAL
      RETURNING VALUE(r_response)    TYPE string.

    METHODS change_method
      IMPORTING
                i_class_name         TYPE yde_aaic_class_name
                i_method_name        TYPE yde_aaic_method_name
                i_description        TYPE yde_aaic_fc_description
                i_transport_request  TYPE yde_aaic_fc_transport_request
                i_package            TYPE yde_aaic_fc_package
                i_s_returning_params TYPE yst_aaic_fc_method_return_par OPTIONAL
                i_t_importing_params TYPE ytt_aaic_fc_method_imp_par OPTIONAL
                i_t_exporting_params TYPE ytt_aaic_fc_method_exp_par OPTIONAL
                i_t_changing_params  TYPE ytt_aaic_fc_method_chang_par OPTIONAL
      RETURNING VALUE(r_response)    TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA _response TYPE string.

    METHODS _add_importing_parameter
      IMPORTING
                i_o_method_definition   TYPE REF TO if_xco_gen_ao_s_fo_c_method
                i_s_importing_parameter TYPE yst_aaic_fc_method_parameter
      RETURNING VALUE(r_error)          TYPE abap_bool.

    METHODS _add_exporting_parameter
      IMPORTING
                i_o_method_definition   TYPE REF TO if_xco_gen_ao_s_fo_c_method
                i_s_exporting_parameter TYPE yst_aaic_fc_method_exp_par
      RETURNING VALUE(r_error)          TYPE abap_bool.

    METHODS _add_changing_parameter
      IMPORTING
                i_o_method_definition  TYPE REF TO if_xco_gen_ao_s_fo_c_method
                i_s_changing_parameter TYPE yst_aaic_fc_method_parameter
      RETURNING VALUE(r_error)         TYPE abap_bool.

    METHODS _add_returning_parameter
      IMPORTING
                i_o_method_definition   TYPE REF TO if_xco_gen_ao_s_fo_c_method
                i_s_returning_parameter TYPE yst_aaic_fc_method_return_par
      RETURNING VALUE(r_error)          TYPE abap_bool.

ENDCLASS.



CLASS ycl_aaic_class_tools IMPLEMENTATION.

  METHOD create.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(lo_specification) = lo_put_operation->for-clas->add_object( l_class_name
      )->set_package( l_package
      )->create_form_specification( ).

    lo_specification->set_short_description( 'Customer implementation' ).

    lo_specification->definition->set_create_visibility( xco_cp_abap_objects=>visibility->public ).

    TRY.

        lo_put_operation->execute( ).

        r_response = |Class `{ l_class_name }` created successfully!|.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        r_response = |Error: the class `{ l_class_name }` was not created.|.

        DATA(lo_findings) = lo_cx_xco_gen_put_exception->findings->for->clas.

        DATA(lt_findings) = lo_findings->get( ).

        LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <ls_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

            r_response = r_response && <lo_message>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD add_method.

    DATA lt_source TYPE STANDARD TABLE OF string.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    DATA(lo_method_definition) = lo_patch_operation_object->for-insert->definition->section-public->add_method( l_method_name
      )->set_short_description( i_description ).

    LOOP AT i_t_importing_params ASSIGNING FIELD-SYMBOL(<ls_importing_params>).

      me->_add_importing_parameter(
        i_o_method_definition   = lo_method_definition
        i_s_importing_parameter = <ls_importing_params>
      ).

    ENDLOOP.

    LOOP AT i_t_exporting_params ASSIGNING FIELD-SYMBOL(<ls_exporting_params>).

      me->_add_exporting_parameter(
        i_o_method_definition   = lo_method_definition
        i_s_exporting_parameter = <ls_exporting_params>
      ).

    ENDLOOP.

    SPLIT i_source AT cl_abap_char_utilities=>newline INTO TABLE lt_source.

    DATA(lo_method_implementation) = lo_patch_operation_object->for-insert->implementation->add_method( l_method_name
      )->set_source( lt_source ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Method `{ l_method_name }` added to class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Method `{ l_method_name }` was not added to class `{ l_class_name }`.|.

        DATA(lo_findings) = lo_cx_xco_gen_patch_exception->findings->for->clas.

        DATA(lt_findings) = lo_findings->get( ).

        LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <ls_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

            r_response = r_response && <lo_message>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD change_method.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    DATA(lo_method_definition) = lo_patch_operation_object->for-update->definition->section-public->add_method( l_method_name ).

    lo_method_definition->for-insert->add_importing_parameter( 'I_P2' )->set_pass_by_reference( )->set_type( xco_cp_abap_dictionary=>data_element( 'ZDE_CJS_EMAIL_FROM' ) ).
    lo_method_definition->for-insert->add_importing_parameter( 'I_S_1' )->set_pass_by_reference( )->set_type( xco_cp_abap_dictionary=>structure( 'ZST_CJS_STRUC_XCO_TEST' ) ).
    lo_method_definition->for-insert->add_importing_parameter( 'I_T_1' )->set_pass_by_reference( )->set_type( xco_cp_abap_dictionary=>table_type( 'ZTT_TEST_CJS_XCO_LIBRARY_12' ) ).

    DATA(lo_method_implementation) = lo_patch_operation_object->for-insert->implementation->add_method( l_method_name
      )->set_source( VALUE #( ( |"BINGO!| ) ) ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Method `{ l_method_name }` added to class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Method `{ l_method_name }` was not added to class `{ l_class_name }`.|.

        DATA(lo_findings) = lo_cx_xco_gen_patch_exception->findings->for->clas.

        DATA(lt_findings) = lo_findings->get( ).

        LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <ls_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

            r_response = r_response && <lo_message>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD _add_importing_parameter.

    DATA(ls_importing_parameter) = i_s_importing_parameter.

    DATA(lo_ddic_tools_utils) = NEW ycl_aaic_ddic_tools_util( ).

    IF ls_importing_parameter-built_in_type IS NOT INITIAL.

      NEW ycl_aaic_ddic_tools_util( )->determine_format(
        EXPORTING
          i_data_type = ls_importing_parameter-built_in_type
          i_length    = ls_importing_parameter-length
          i_decimals  = ls_importing_parameter-decimals
        IMPORTING
          e_o_format  = DATA(lo_format)
          e_error     = DATA(l_error)
      ).

      IF l_error IS NOT INITIAL.
        RETURN.
      ENDIF.

      IF lo_format IS NOT BOUND.
        me->_response = |The data type { ls_importing_parameter-built_in_type } is incorrect or invalid. Only ABAP built-in types are allowed. { NEW ycl_aaic_ddic_tools_util( )->get_built_in_types_response( ) }|.
        RETURN.
      ENDIF.

      i_o_method_definition->add_importing_parameter( CONV #( ls_importing_parameter-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-built_in->string
        )->set_optional( ls_importing_parameter-optional
        )->set_default_value( ls_importing_parameter-default_value ).

    ELSEIF ls_importing_parameter-type IS NOT INITIAL.

      DATA(l_object_type) = lo_ddic_tools_utils->get_object_type( ls_importing_parameter-type ).

      CASE l_object_type.

        WHEN 'DTEL'.

          i_o_method_definition->add_importing_parameter( CONV #( ls_importing_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap_dictionary=>data_element( CONV #( ls_importing_parameter-type ) )
            )->set_optional( ls_importing_parameter-optional
            )->set_default_value( ls_importing_parameter-default_value ).

        WHEN 'TABL'.

          i_o_method_definition->add_importing_parameter( CONV #( ls_importing_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap_dictionary=>structure( CONV #( ls_importing_parameter-type ) )
            )->set_optional( ls_importing_parameter-optional ).

        WHEN 'TTYP'.

          i_o_method_definition->add_importing_parameter( CONV #( ls_importing_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap_dictionary=>table_type( CONV #( ls_importing_parameter-type ) )
            )->set_optional( ls_importing_parameter-optional ).

        WHEN 'CLAS'.

          i_o_method_definition->add_importing_parameter( CONV #( ls_importing_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap=>class( CONV #( ls_importing_parameter-type ) )
            )->set_optional( ls_importing_parameter-optional ).

        WHEN 'INTF'.

          i_o_method_definition->add_importing_parameter( CONV #( ls_importing_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap=>interface( CONV #( ls_importing_parameter-type ) )
            )->set_optional( ls_importing_parameter-optional ).

      ENDCASE.

    ENDIF.

  ENDMETHOD.

  METHOD _add_exporting_parameter.

    DATA(ls_exporting_parameter) = i_s_exporting_parameter.

    DATA(lo_ddic_tools_utils) = NEW ycl_aaic_ddic_tools_util( ).

    IF ls_exporting_parameter-built_in_type IS NOT INITIAL.

      i_o_method_definition->add_exporting_parameter( CONV #( ls_exporting_parameter-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-built_in->string ).

    ELSEIF ls_exporting_parameter-type IS NOT INITIAL.

      DATA(l_object_type) = lo_ddic_tools_utils->get_object_type( ls_exporting_parameter-type ).

      CASE l_object_type.

        WHEN 'DTEL'.

          i_o_method_definition->add_exporting_parameter( CONV #( ls_exporting_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap_dictionary=>data_element( CONV #( ls_exporting_parameter-type ) ) ).

        WHEN 'TABL'.

          i_o_method_definition->add_exporting_parameter( CONV #( ls_exporting_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap_dictionary=>structure( CONV #( ls_exporting_parameter-type ) ) ).

        WHEN 'TTYP'.

          i_o_method_definition->add_exporting_parameter( CONV #( ls_exporting_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap_dictionary=>table_type( CONV #( ls_exporting_parameter-type ) ) ).

        WHEN 'CLAS'.

          i_o_method_definition->add_exporting_parameter( CONV #( ls_exporting_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap=>class( CONV #( ls_exporting_parameter-type ) ) ).

        WHEN 'INTF'.

          i_o_method_definition->add_exporting_parameter( CONV #( ls_exporting_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap=>interface( CONV #( ls_exporting_parameter-type ) ) ).

      ENDCASE.

    ENDIF.

  ENDMETHOD.

  METHOD _add_changing_parameter.

    DATA(ls_changing_parameter) = i_s_changing_parameter.

    DATA(lo_ddic_tools_utils) = NEW ycl_aaic_ddic_tools_util( ).

    IF ls_changing_parameter-built_in_type IS NOT INITIAL.

      i_o_method_definition->add_changing_parameter( CONV #( ls_changing_parameter-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-built_in->string
        )->set_optional( ls_changing_parameter-optional
        )->set_default_value( ls_changing_parameter-default_value ).

    ELSEIF ls_changing_parameter-type IS NOT INITIAL.

      DATA(l_object_type) = lo_ddic_tools_utils->get_object_type( ls_changing_parameter-type ).

      CASE l_object_type.

        WHEN 'DTEL'.

          i_o_method_definition->add_changing_parameter( CONV #( ls_changing_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap_dictionary=>data_element( CONV #( ls_changing_parameter-type ) )
            )->set_optional( ls_changing_parameter-optional
            )->set_default_value( ls_changing_parameter-default_value ).

        WHEN 'TABL'.

          i_o_method_definition->add_changing_parameter( CONV #( ls_changing_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap_dictionary=>structure( CONV #( ls_changing_parameter-type ) )
            )->set_optional( ls_changing_parameter-optional ).

        WHEN 'TTYP'.

          i_o_method_definition->add_changing_parameter( CONV #( ls_changing_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap_dictionary=>table_type( CONV #( ls_changing_parameter-type ) )
            )->set_optional( ls_changing_parameter-optional ).

        WHEN 'CLAS'.

          i_o_method_definition->add_changing_parameter( CONV #( ls_changing_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap=>class( CONV #( ls_changing_parameter-type ) )
            )->set_optional( ls_changing_parameter-optional ).

        WHEN 'INTF'.

          i_o_method_definition->add_changing_parameter( CONV #( ls_changing_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap=>interface( CONV #( ls_changing_parameter-type ) )
            )->set_optional( ls_changing_parameter-optional ).

      ENDCASE.

    ENDIF.

  ENDMETHOD.

  METHOD _add_returning_parameter.

    DATA(ls_returning_parameter) = i_s_returning_parameter.

    DATA(lo_ddic_tools_utils) = NEW ycl_aaic_ddic_tools_util( ).

    IF ls_returning_parameter-built_in_type IS NOT INITIAL.

      i_o_method_definition->add_changing_parameter( CONV #( ls_returning_parameter-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-built_in->string ).

    ELSEIF ls_returning_parameter-type IS NOT INITIAL.

      DATA(l_object_type) = lo_ddic_tools_utils->get_object_type( ls_returning_parameter-type ).

      CASE l_object_type.

        WHEN 'DTEL'.

          i_o_method_definition->add_returning_parameter( CONV #( ls_returning_parameter-name )
            )->set_type( xco_cp_abap_dictionary=>data_element( CONV #( ls_returning_parameter-type ) ) ).

        WHEN 'TABL'.

          i_o_method_definition->add_changing_parameter( CONV #( ls_returning_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap_dictionary=>structure( CONV #( ls_returning_parameter-type ) ) ).

        WHEN 'TTYP'.

          i_o_method_definition->add_changing_parameter( CONV #( ls_returning_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap_dictionary=>table_type( CONV #( ls_returning_parameter-type ) ) ).

        WHEN 'CLAS'.

          i_o_method_definition->add_changing_parameter( CONV #( ls_returning_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap=>class( CONV #( ls_returning_parameter-type ) ) ).

        WHEN 'INTF'.

          i_o_method_definition->add_changing_parameter( CONV #( ls_returning_parameter-name )
            )->set_pass_by_reference(
            )->set_type( xco_cp_abap=>interface( CONV #( ls_returning_parameter-type ) ) ).

      ENDCASE.

    ENDIF.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    DATA(l_create) = abap_false.
    DATA(l_add_method) = abap_true.

    IF l_create = abap_true.

      l_response = me->create(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_description       = 'XCO ABAP Class generation'
          i_transport_request = 'TRLK900008'
          i_package           = 'ZCHRJS'
      ).

    ENDIF.

    IF l_add_method = abap_true.

      l_response = me->add_method(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'M4'
          i_description       = 'Method 4'
          i_transport_request = 'TRLK900008'
          i_package           = 'ZCHRJS'
          i_source            = |IF 1 = 2. { cl_abap_char_utilities=>newline } ENDIF.|
          i_t_importing_params = VALUE #( ( name = 'P1' type = 'YDE_AAIC_API' ) )
        ).

    ENDIF.

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
